module Type.InferMonad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Supply
import Control.Monad.Writer
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Name
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as TLB
import Lens.Micro
import Poly.Pretty
import Prettyprinter
import Test.QuickCheck (Arbitrary)
import TextShow
import Type.Env
import Type.Types
import Prelude hiding (lookup)

type Constraint = (Type, Type)

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable Name
  deriving (Eq, Show)

instance TextShow TypeError where
  showb (InfiniteType v t) =
    mconcat
      [ "Cannot construct the infinite type: ",
        pprb v,
        " = ",
        pprb t
      ]
  showb (UnificationFail a b) = mconcat ["Cannot unify type ", pprb a, " with ", pprb b]
  showb (UnboundVariable x) = mconcat ["Name ", TLB.fromText x, " is not in scope"]

type Infer a =
  ExceptT
    TypeError
    (StateT Env (WriterT (DList Constraint) (Supply TVar)))
    a

type MonadInfer m =
  ( MonadState Env m,
    MonadSupply TVar m,
    MonadWriter (DList Constraint) m,
    MonadError TypeError m
  )

type MonadSolve m = (MonadError TypeError m)

-- | Solve for the toplevel type of an expression in a given environment
runInfer :: Env -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = do
  let (ty, res) =
        fromJust
          ( evalSupply
              ( runWriterT
                  ( evalStateT
                      (runExceptT m)
                      env
                  )
              )
              unboundTVarSupply
          )
  ty' <- ty
  return (ty', DL.toList res)

withEnv :: MonadState Env m => Env -> m a -> m a
withEnv env action = do
  orig <- get
  put env
  a <- action
  put orig
  return a

bindName :: MonadState Env m => (Name, Scheme) -> m a -> m a
bindName newNames = bindNames [newNames]

bindNames :: MonadState Env m => Map Name Scheme -> m a -> m a
bindNames newNames action = do
  orig <- get
  modify (names %~ Map.union newNames)
  a <- action
  modify (names .~ (orig ^. names))
  return a

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Arbitrary)

singleSubst :: TVar -> Type -> Subst
singleSubst tv ty = Subst $ Map.singleton tv ty

emptySubst :: Subst
emptySubst = Subst Map.empty

instance PP Subst where
  pp (Subst s) = "{" <+> sep [pp k <+> "-->" <+> pp v | (k, v) <- Map.toList s] <+> "}"

instance Semigroup Subst where
  subst1@(Subst s1) <> subst2 = Subst (s1 `Map.union` s2)
    where
      Subst s2 = subst1 @@ subst2

instance Monoid Subst where
  mappend = (<>)

  mempty = emptySubst

class Substitutable a where
  apply :: Subst -> a -> a
  apply = (@@)

  (@@) :: Subst -> a -> a
  (@@) = apply

  {-# MINIMAL apply | (@@) #-}

instance Substitutable Type where
  _ @@ (TCon a) = TCon a
  (Subst s) @@ t@(TVar a) = Map.findWithDefault t a s
  s @@ (t1 :->: t2) = (s @@ t1) :->: (s @@ t2)
  _ @@ t@(ADTTCon _) = t

instance Substitutable Scheme where
  (Subst s) @@ (Forall as t) = Forall as $ s' @@ t
    where
      s' = Subst $ foldr Map.delete s as

instance Substitutable Constraint where
  s @@ (t1, t2) = (s @@ t1, s @@ t2)

instance Substitutable a => Substitutable [a] where
  apply = map . apply

instance Substitutable Env where
  s @@ env = env & names %~ Map.map (apply s)

instance Substitutable Subst where
  s @@ (Subst target) = Subst ((s @@) <$> target)

class Types a where
  ftv :: a -> Set TVar

instance Types Type where
  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 :->: t2) = ftv t1 `Set.union` ftv t2
  ftv ADTTCon {} = Set.empty

instance Types Scheme where
  ftv (Forall as t) = ftv t `Set.difference` as

instance Types Constraint where
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Types Env where
  ftv env = ftv $ Map.elems $ env ^. names

instance Types a => Types [a] where
  ftv = foldr (Set.union . ftv) Set.empty
