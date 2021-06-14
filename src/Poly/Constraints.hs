module Poly.Constraints where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Poly.Syntax
import Poly.Type
import Test.QuickCheck (Arbitrary)

type TypeEnv = Map Name Scheme

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = Either TypeError a

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Arbitrary)

instance Semigroup Subst where
  subst1@(Subst s1) <> subst2 = Subst (s1 `Map.union` s2)
    where
      Subst s2 = apply subst1 subst2

instance Monoid Subst where
  mappend = (<>)

  mempty = Subst Map.empty

newtype InferState = InferState {count :: Int}

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable Name
  deriving (Eq, Show)

type Infer a = ReaderT TypeEnv (StateT InferState (Either TypeError)) a

type InferM m =
  ( MonadReader TypeEnv m,
    MonadState InferState m,
    MonadError TypeError m
  )

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = solver st
  where
    st = (emptySubst, cs)

lookupEnv ::
  InferM m =>
  Name ->
  m Type
lookupEnv x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

instantiate :: InferM m => Scheme -> m Type
instantiate (Forall as t) = do
  let sA = Map.fromSet (const fresh) as
  s <- sequenceA sA
  return $ apply (Subst s) t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = ftv t `Set.difference` ftv env

fresh :: InferM m => m Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV (letters !! count s)

letters :: [Text]
letters = T.pack <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

inEnv :: MonadReader TypeEnv m => (Name, Scheme) -> m a -> m a
inEnv (x, sc) m = do
  let scope e = Map.insert x sc e
  local scope m

infer :: InferM m => Expr -> m (Type, [Constraint])
infer expr = case expr of
  Lit (LInt _) -> return (TCon TInt, [])
  Lit (LBool _) -> return (TCon TBool, [])
  Var x -> do
    t <- lookupEnv x
    return (t, [])
  Lam x e -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall Set.empty tv) (infer e)
    return (tv `TArr` t, c)
  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])
  Let x e1 e2 -> do
    env <- ask
    (t1, c1) <- infer e1
    case runSolve c1 of
      Left err -> throwError err
      Right sub -> do
        let sc = generalize (apply sub env) (apply sub t1)
        (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
        return (t2, c1 ++ c2)
  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 ++ [(tv `TArr` tv, t1)])
  Op op e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TArr` (t2 `TArr` tv)
        u2 = ops Map.! op
    return (tv, c1 ++ c2 ++ [(u1, u2)])
  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    return (t2, c1 ++ c2 ++ c3 ++ [(t1, TCon TBool), (t2, t3)])
  _ -> error "todo"

ops :: Map BinOp Type
ops =
  Map.fromList
    [ (Add, intBinFun),
      (Mul, intBinFun),
      (Sub, intBinFun),
      (Eql, TCon TInt `TArr` (TCon TInt `TArr` TCon TBool))
    ]

emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) =
  Subst $
    Map.map
      ( apply
          ( Subst
              s1
          )
      )
      s2
      `Map.union` s1

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Solve Subst
bind a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2) : cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s env = Map.map (apply s) env
  ftv env = ftv $ Map.elems env

instance Substitutable Subst where
  apply s (Subst target) = Subst (apply s <$> target)

substTConProp :: Subst -> TCon -> Bool
substTConProp s tcon = apply s t == t
  where
    t = TCon tcon

substAssociative :: (Subst, Subst, Subst) -> Subst -> Bool
substAssociative (s1, s2, s3) st = all (== (vals !! 1)) vals
  where
    vals = ($ st) <$> tests

    tests :: [Subst -> Subst]
    tests =
      [ apply ((s1 <> s2) <> s3),
        apply (s1 <> (s2 <> s3))
      ]

equalTypesUnifyProp :: Type -> Bool
equalTypesUnifyProp t = unifies t t == Right emptySubst
