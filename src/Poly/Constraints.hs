module Poly.Constraints where

import Control.Monad.Except
import Control.Monad.RWS
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Debug.Trace
import Poly.Pretty
import Poly.Syntax
import Poly.Type
import Prettyprinter
import Test.QuickCheck (Arbitrary)
import TextShow

type TypeEnv = Map Name Scheme

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = Either TypeError a

type SolveM m = (MonadError TypeError m)

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Arbitrary)

instance PP Subst where
  pp (Subst s) = "{" <+> sep [pp k <+> "-->" <+> pp v | (k, v) <- Map.toList s] <+> "}"

instance Semigroup Subst where
  subst1@(Subst s1) <> subst2 = Subst (s1 `Map.union` s2)
    where
      Subst s2 = subst1 @@ subst2

instance Monoid Subst where
  mappend = (<>)

  mempty = Subst Map.empty

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState {count = 0}

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable Name
  deriving (Eq, Show)

instance TextShow TypeError where
  showb (InfiniteType v t) = mconcat ["Cannot construct the infinite type: ", pprb v, " = ", pprb t]
  showb (UnificationFail a b) = mconcat ["Cannot unify type ", pprb a, " with ", pprb b]
  showb (UnboundVariable x) = mconcat ["Name ", TLB.fromText x, " is not in scope"]

-- type Infer a = ReaderT TypeEnv (StateT InferState (Either TypeError)) a
type Infer a =
  ExceptT
    TypeError
    ( RWS TypeEnv (DList Constraint) InferState
    )
    a

type InferM m =
  ( MonadReader TypeEnv m,
    MonadState InferState m,
    MonadWriter (DList Constraint) m,
    MonadError TypeError m
  )

-- | Solve for the toplevel type of an expression in a given environment
runInfer :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = do
  let (cs, res) = evalRWS (runExceptT m) env initInfer
  cs' <- cs
  return (cs', DL.toList res)

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env ex = do
  (ty, cs) <- runInfer env (infer ex)
  subst <- runSolve cs
  return $ closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Map.empty

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (Set.fromList simplified) (normtype body)
  where
    pool = map TV letters

    ord = Map.fromList $ zip (Set.toList ftvs) simplified
    simplified = take (Set.size ftvs) pool
    ftvs = ftv body

    normtype (a :->: b) = normtype a :->: normtype b
    normtype (TCon a) = TCon a
    normtype (TVar a) = TVar $ ord Map.! a

-- | Run the constraint solver
runSolve :: SolveM m => [Constraint] -> m Subst
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

fresh :: MonadState InferState m => m Type
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

constrain, (->>) :: MonadWriter (DList Constraint) m => Type -> Type -> m ()
constrain t1 t2 = do
  tell [(t1, t2)]
(->>) = constrain

infer :: InferM m => Expr -> m Type
infer expr = case expr of
  Lit l -> inferLit l
  Var x -> lookupEnv x
  Lam x e -> do
    tv <- fresh
    t <- inEnv (x, Forall [] tv) (infer e)
    return $ tv :->: t
  App e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- fresh
    constrain t1 (t2 :->: tv)
    return tv
  Let x e1 e2 -> inferLet x e1 e2
  Fix e1 -> do
    t1 <- infer e1
    tv <- fresh
    constrain (tv :->: tv) t1
    return tv
  Op op e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- fresh
    let u1 = t1 :->: t2 :->: tv
        u2 = ops Map.! op
    constrain u1 u2
    return tv
  If cond tr fl -> do
    t1 <- infer cond
    t2 <- joinTy tr fl
    constrain t1 (TCon TBool)
    return t2

joinTy :: InferM m => Expr -> Expr -> m Type
joinTy e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  constrain t1 t2
  return t2

inferLit :: InferM m => Lit -> m Type
inferLit lit = return (TCon $ litTy lit)

litTy :: Lit -> TCon
litTy (LInt _) = TInt
litTy (LBool _) = TBool
litTy (LStr _) = TStr
litTy (LChar _) = TChar

inferLet :: InferM m => Name -> Expr -> Expr -> m Type
inferLet x e e' = do
  env <- ask
  (t1, c1) <- listen (infer e)
  sub <- runSolve $ DL.toList c1
  let sc = generalize (apply sub env) (apply sub t1)
  inEnv (x, sc) $ local (apply sub) (infer e')

ops :: Map BinOp Type
ops =
  Map.fromList
    [ (Add, intBinFun),
      (Mul, intBinFun),
      (Sub, intBinFun),
      (Eql, tInt :->: tInt :->: tBool)
    ]

emptySubst :: Subst
emptySubst = mempty

unifies :: SolveM m => Type -> Type -> m Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (t1 :->: ts1) (t2 :->: ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifies (su1 @@ ts1) (su1 @@ ts2)
  return (su2 <> su1)
unifies t1 t2 = throwError $ UnificationFail t1 t2

bind :: SolveM m => TVar -> Type -> m Subst
bind a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: FTV a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- Unification solver
solver :: SolveM m => Unifier -> m Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2) : cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 <> su, apply su1 cs0)

class Substitutable a where
  apply :: Subst -> a -> a
  apply = (@@)

  (@@) :: Subst -> a -> a
  (@@) = apply

  {-# MINIMAL apply | (@@) #-}

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 :->: t2) = apply s t1 :->: apply s t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)

instance Substitutable a => Substitutable [a] where
  apply = map . apply

instance Substitutable TypeEnv where
  apply s env = Map.map (apply s) env

instance Substitutable Subst where
  apply s (Subst target) = Subst (apply s <$> target)

class FTV a where
  ftv :: a -> Set TVar

instance FTV Type where
  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 :->: t2) = ftv t1 `Set.union` ftv t2

instance FTV Scheme where
  ftv (Forall as t) = ftv t `Set.difference` as

instance FTV Constraint where
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance FTV TypeEnv where
  ftv env = ftv $ Map.elems env

instance FTV a => FTV [a] where
  ftv = foldr (Set.union . ftv) Set.empty

substTConProp :: Subst -> TCon -> Bool
substTConProp s tcon = apply s t == t
  where
    t = TCon tcon

ftvTConProp :: TCon -> Bool
ftvTConProp t = null (ftv $ TCon t)

substAssociative :: (Subst, Subst, Subst) -> Subst -> Bool
substAssociative (s1, s2, s3) st = all (== (vals !! 1)) vals
  where
    vals = ($ st) <$> tests

    tests :: [Subst -> Subst]
    tests =
      [ apply ((s1 <> s2) <> s3),
        apply (s1 <> s2 <> s3)
      ]

equalTypesUnifyProp :: Type -> Bool
equalTypesUnifyProp t = unifies t t == Right emptySubst

tVarFTVProp :: TVar -> Bool
tVarFTVProp tv = ftv (TVar tv) == [tv]

tArrFTVProp :: Type -> Type -> Bool
tArrFTVProp t1 t2 = ftv (t1 :->: t2) == ftv t1 `Set.union` ftv t2
