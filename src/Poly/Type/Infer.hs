module Poly.Type.Infer
  ( inferExpr,
    inferTop,
    TypeError,
    emptyTypeEnv,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Either.Combinators
import Data.Function
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Poly.Pretty
import Poly.QQ
import Poly.Syntax
import Poly.Type.Types
import TextShow

newtype TypeEnv = TypeEnv (Map Name Scheme)

newtype Unique = Unique {count :: Int}

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable Name
  deriving (Eq, Show)

instance TextShow TypeError where
  showb (InfiniteType v t) = mconcat ["Cannot construct the infinite type: ", pprb v, " = ", pprb t]
  showb (UnificationFail a b) = mconcat ["Cannot unify type ", pprb a, " with ", pprb b]
  showb (UnboundVariable x) = mconcat ["Name ", TLB.fromText x, " is not in scope"]

type Subst = Map.Map TVar Type

type InferM m =
  ( MonadError TypeError m,
    MonadState Unique m
  )

type Infer a = ExceptT TypeError (State Unique) a

data Constraint
  = CEqual Type Type
  | CGen Type Type
  | CInst Type Type

emptySubst :: Subst
emptySubst = Map.empty

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = apply s1 <$> s2 `Map.union` s1

initUnique :: Unique
initUnique = Unique 0

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where
    sc = generalize emptyTypeEnv (apply sub ty)

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (Set.fromList (fmap snd ord)) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a) = [a]
    fv (a :->: b) = fv a ++ fv b
    fv (TCon _) = []

    normtype (a :->: b) = normtype a :->: normtype b
    normtype (TCon a) = TCon a
    normtype (TVar a) =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = evalState (runExceptT m) initUnique & mapRight closeOver

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 :->: t2) = apply s t1 :->: apply s t2

  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 :->: t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr Map.delete s as

  ftv (Forall as t) = ftv t `Set.difference` as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ apply s <$> env

  ftv (TypeEnv env) = ftv $ Map.elems env

letters :: [Text]
letters = T.pack <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

fresh :: InferM m => m Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV $ letters !! count s

unify :: InferM m => Type -> Type -> m Subst
unify (l :->: r) (l' :->: r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return $ s2 `compose` s1
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return emptySubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: InferM m => TVar -> Type -> m Subst
bind a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

instantiate :: InferM m => Scheme -> m Type
instantiate (Forall as t) = do
  let asA = Map.fromSet (const fresh) as
  s <- sequenceA asA
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = ftv t `Set.difference` ftv env

infer :: InferM m => TypeEnv -> Expr -> m (Subst, Type)
infer env ex = case ex of
  Var x -> lookupEnv env x
  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall Set.empty tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv :->: t1)
  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (t2 :->: tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t' = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s1 `compose` s2, t2)
  If cond tr fl -> do
    (s1, t1) <- infer env cond
    (s2, t2) <- infer env tr
    (s3, t3) <- infer env fl
    s4 <- unify t1 $ TCon TBool
    s5 <- unify t2 t3
    return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
  Fix e1 -> do
    (s1, t) <- infer env e1
    tv <- fresh
    s2 <- unify (tv :->: tv) t
    return (s2, apply s1 tv)
  Bin op e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    tv <- fresh
    s3 <- unify (t1 :->: t2 :->: tv) (ops Map.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)
  Lit l -> inferLit l

inferLit :: InferM m => Lit -> m (Subst, Type)
inferLit l = return (emptySubst, TCon $ litTCon l)

litTCon :: Lit -> TCon
litTCon (LInt _) = TInt
litTCon (LBool _) = TInt
litTCon (LStr _) = TStr
litTCon (LChar _) = TChar

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

ops :: Map BinOp Type
ops =
  Map.fromList
    [ (Add, [ty|Int -> Int -> Int|]),
      (Mul, [ty|Int -> Int -> Int|]),
      (Sub, [ty|Int -> Int -> Int|]),
      (Eql, [ty|Int -> Int -> Bool|])
    ]

lookupEnv :: InferM m => TypeEnv -> Name -> m (Subst, Type)
lookupEnv (TypeEnv env) x = do
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> do
      t <- instantiate s
      return (emptySubst, t)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [Decl] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env (DeclExpr ex : xs) = do
  _ <- inferExpr env ex
  inferTop env xs
inferTop env (Decl name ex : xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs
