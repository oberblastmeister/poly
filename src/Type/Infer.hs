module Type.Infer
  ( inferExpr,
    TypeError (..),
    unify,
    emptySubst,
    generalize,
    Substitutable (..),
    Types (..),
    Subst (..),
  )
where

import AST.Expr
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
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

type Unifier = (Subst, [Constraint])

type MonadSolve m = (MonadError TypeError m)

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

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable Name
  deriving (Eq, Show)

instance TextShow TypeError where
  showb (InfiniteType v t) = mconcat ["Cannot construct the infinite type: ", pprb $ normtype $ TVar v, " = ", pprb $ normtype t]
  showb (UnificationFail a b) = mconcat ["Cannot unify type ", pprb a, " with ", pprb b]
  showb (UnboundVariable x) = mconcat ["Name ", TLB.fromText x, " is not in scope"]

type Infer a =
  ExceptT
    TypeError
    (ReaderT Env (WriterT (DList Constraint) (Supply TVar)))
    a

type InferM m =
  ( MonadReader Env m,
    MonadSupply TVar m,
    MonadWriter (DList Constraint) m,
    MonadError TypeError m
  )

-- | Solve for the toplevel type of an expression in a given environment
runInfer :: Env -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = do
  let (ty, res) =
        fromJust
          ( evalSupply
              ( runWriterT
                  ( runReaderT
                      (runExceptT m)
                      env
                  )
              )
              unboundTVarSupply
          )
  ty' <- ty
  return (ty', DL.toList res)

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: (MonadError TypeError m) => Env -> Expr -> m Scheme
inferExpr env ex = liftEither $ normalize <$> inferExpr' env ex

inferExpr' :: (MonadError TypeError m) => Env -> Expr -> m Scheme
inferExpr' env ex = liftEither $ do
  (ty, cs) <- liftEither $ runInfer env (infer ex)
  su <- runSolve cs
  return $ generalize (su @@ env) $ su @@ ty

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (Set.fromList simplified) (normtype' ord body)
  where
    pool = namedTVarSupply

    ord = Map.fromList $ zip (Set.toList ftvs) simplified
    simplified = take (Set.size ftvs) pool
    ftvs = ftv body

normtype ty = normtype' ord ty
  where
    pool = namedTVarSupply

    ord = Map.fromList $ zip (Set.toList ftvs) simplified
    simplified = take (Set.size ftvs) pool
    ftvs = ftv ty

normtype' ord (a :->: b) = normtype' ord a :->: normtype' ord b
normtype' _ (TCon a) = TCon a
normtype' ord (TVar a) = TVar $ ord Map.! a
normtype' _ t@(ADTTCon _) = t

-- | Run the constraint solver
runSolve :: MonadSolve m => [Constraint] -> m Subst
runSolve cs = solver st
  where
    st = (emptySubst, cs)

instantiate :: MonadSupply TVar m => Scheme -> m Type
instantiate (Forall as t) = do
  let sA = Map.fromSet (const fresh) as
  s <- sequenceA sA
  return $ Subst s @@ t

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t
  where
    as = ftv t `Set.difference` ftv env

fresh :: MonadSupply TVar m => m Type
fresh = TVar <$> supply

inEnv :: MonadReader Env m => (Name, Scheme) -> m a -> m a
inEnv (x, sc) m = do
  let scope e = addScheme e (x, sc)
  local scope m

constrain :: MonadWriter (DList Constraint) m => Type -> Type -> m ()
constrain t1 t2 = tell [(t1, t2)]

infer :: InferM m => Expr -> m Type
infer = \case
  Lit l -> inferLit l
  Var x -> inferVar x
  Lam x e -> inferLam x e
  App e1 e2 -> inferApp e1 e2
  Let x e1 e2 -> inferLet x e1 e2
  Fix e -> inferFix e
  Bin op e1 e2 -> inferOp op e1 e2
  If cond tr fl -> inferIf cond tr fl
  Typed e t -> inferTyped e t

joinTy :: InferM m => Expr -> Expr -> m Type
joinTy e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  constrain t1 t2
  return t2

inferLit :: InferM m => Lit -> m Type
inferLit lit = return $ TCon $ litTy lit

litTy :: Lit -> TCon
litTy (LInt _) = TInt
litTy (LBool _) = TBool
litTy (LStr _) = TStr
litTy (LChar _) = TChar

inferLet :: InferM m => Name -> Expr -> Expr -> m Type
inferLet x e e' = do
  env <- ask
  (t, cs) <- runWriterT $ infer e
  su <- runSolve $ DL.toList cs
  let env' = su @@ env
  let sc = generalize env' $ su @@ t
  local (const env') $ inEnv (x, sc) $ infer e'

inferVar :: InferM m => Name -> m Type
inferVar x = do
  env <- ask
  case lookupScheme x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

inferApp :: InferM m => Expr -> Expr -> m Type
inferApp e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  tv <- fresh
  constrain t1 (t2 :->: tv)
  return tv

inferLam :: InferM m => Name -> Expr -> m Type
inferLam x e = do
  tv <- fresh
  t <- inEnv (x, Forall [] tv) (infer e)
  return $ tv :->: t

inferFix :: InferM m => Expr -> m Type
inferFix e = do
  t1 <- infer e
  tv <- fresh
  constrain (tv :->: tv) t1
  return tv

inferOp :: InferM m => BinOp -> Expr -> Expr -> m Type
inferOp op e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  tv <- fresh
  let u1 = t1 :->: t2 :->: tv
      u2 = ops Map.! op
  constrain u1 u2
  return tv

inferIf :: InferM m => Expr -> Expr -> Expr -> m Type
inferIf cond tr fl = do
  t1 <- infer cond
  t2 <- joinTy tr fl
  constrain t1 (TCon TBool)
  return t2

inferTyped :: InferM m => Expr -> Type -> m Type
inferTyped e t = do
  t' <- infer e
  constrain t' t
  return t'

ops :: Map BinOp Type
ops =
  Map.fromList
    [ (Add, intBinFun),
      (Mul, intBinFun),
      (Sub, intBinFun),
      (Eql, tInt :->: tInt :->: tBool)
    ]

unify :: MonadSolve m => Type -> Type -> m Subst
unify t1 t2 | t1 == t2 = return emptySubst
unify (TVar v) t = v `bind` t
unify t (TVar v) = v `bind` t
unify (t1 :->: ts1) (t2 :->: ts2) = do
  su1 <- unify t1 t2
  su2 <- unify (su1 @@ ts1) (su1 @@ ts2)
  return (su2 <> su1)
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: MonadSolve m => TVar -> Type -> m Subst
bind a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ singleSubst a t

occursCheck :: Types a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- Unification solver
solver :: MonadSolve m => Unifier -> m Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2) : cs0) -> do
      su1 <- unify t1 t2
      solver (su1 <> su, su1 @@ cs0)

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
  s @@ env = env & types %~ Map.map (apply s)

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
  ftv env = ftv $ Map.elems $ env ^. types

instance Types a => Types [a] where
  ftv = foldr (Set.union . ftv) Set.empty
