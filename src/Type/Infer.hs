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
import Control.Monad.Supply
import Control.Monad.Writer
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Name
import qualified Data.Set as Set
import Lens.Micro
import Type.Env
import Type.InferMonad
import Type.Types
import Type.Unify
import Prelude hiding (lookup)

type Unifier = (Subst, [Constraint])

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

instantiateUser :: (MonadSupply TVar m, MonadState Env m) => Type -> m Type
instantiateUser (TVar v) = TVar <$> instantiateUserVar v
instantiateUser t@(TCon _) = return t
instantiateUser t@(ADTTCon _) = return t
instantiateUser (t1 :->: t2) = (:->:) <$> instantiateUser t1 <*> instantiateUser t2

instantiateUserVar :: (MonadSupply TVar m, MonadState Env m) => TVar -> m TVar
instantiateUserVar tv@(TVUnbound _) = return tv
instantiateUserVar (TVNamed x) = do
  tvs <- gets (^. tvars)
  maybe
    ( do
        v <- supply
        modify (tvars %~ Map.insert x v)
        return v
    )
    return
    (Map.lookup x tvs)

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t
  where
    as = ftv t `Set.difference` ftv env

fresh :: MonadSupply TVar m => m Type
fresh = TVar <$> supply

constrain :: MonadWriter (DList Constraint) m => Type -> Type -> m ()
constrain t1 t2 = tell [(t1, t2)]

infer :: MonadInfer m => Expr -> m Type
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

joinTy :: MonadInfer m => Expr -> Expr -> m Type
joinTy e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  constrain t1 t2
  return t2

inferLit :: MonadInfer m => Lit -> m Type
inferLit lit = return $ TCon $ litTy lit

litTy :: Lit -> TCon
litTy (LInt _) = TInt
litTy (LBool _) = TBool
litTy (LStr _) = TStr
litTy (LChar _) = TChar

inferLet :: MonadInfer m => Name -> Expr -> Expr -> m Type
inferLet x e e' = do
  env <- get
  (t, cs) <- runWriterT $ infer e
  su <- runSolve $ DL.toList cs
  let env' = su @@ env
  let sc = generalize env' $ su @@ t
  withEnv env' $ bindName (x, sc) $ infer e'

inferVar :: MonadInfer m => Name -> m Type
inferVar x = do
  env <- get
  case lookupScheme x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

inferApp :: MonadInfer m => Expr -> Expr -> m Type
inferApp e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  tv <- fresh
  constrain t1 (t2 :->: tv)
  return tv

inferLam :: MonadInfer m => Name -> Expr -> m Type
inferLam x e = do
  tv <- fresh
  t <- bindName (x, Forall [] tv) (infer e)
  return $ tv :->: t

inferFix :: MonadInfer m => Expr -> m Type
inferFix e = do
  t1 <- infer e
  tv <- fresh
  constrain (tv :->: tv) t1
  return tv

inferOp :: MonadInfer m => BinOp -> Expr -> Expr -> m Type
inferOp op e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  tv <- fresh
  let u1 = t1 :->: t2 :->: tv
      u2 = ops Map.! op
  constrain u1 u2
  return tv

inferIf :: MonadInfer m => Expr -> Expr -> Expr -> m Type
inferIf cond tr fl = do
  t1 <- infer cond
  t2 <- joinTy tr fl
  constrain t1 (TCon TBool)
  return t2

inferTyped :: MonadInfer m => Expr -> Type -> m Type
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

-- Unification solver
solver :: MonadSolve m => Unifier -> m Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2) : cs0) -> do
      su1 <- unify t1 t2
      solver (su1 <> su, su1 @@ cs0)
