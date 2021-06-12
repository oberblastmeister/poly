module Poly.Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.Either.Combinators
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Poly.Syntax
import Poly.Type

data Scheme = Forall [TVar] Type

newtype TypeEnv = TypeEnv (Map Name Scheme)

newtype Unique = Unique {count :: Int}

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable Name

type Subst = Map.Map TVar Type

type InferM m =
  ( MonadError TypeError m,
    MonadState Unique m
  )

type Infer a = ExceptT TypeError (State Unique) a

emptySubst :: Subst
emptySubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = apply s1 <$> s2 `Map.union` s1

initUnique :: Unique
initUnique = Unique 0

closeOver = undefined

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = evalState (runExceptT m) initUnique & mapRight closeOver

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr Map.delete s as

  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

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

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unify :: InferM m => Type -> Type -> m Subst
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return $ s2 `compose` s1
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: InferM m => TVar -> Type -> m Subst
bind a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

instantiate :: InferM m => Scheme -> m Type
instantiate (Forall as t) = do
  as' <- traverse (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

infer :: InferM m => TypeEnv -> Expr -> m (Subst, Type)
infer env ex = case ex of
  Var x -> lookupEnv env x
  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArr` t1)
  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (TArr t2 tv)
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
    s4 <- unify t1 typeBool
    s5 <- unify t2 t3
    return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
  Fix e1 -> do
    (s1, t) <- infer env e1
    tv <- fresh
    s2 <- unify (TArr tv tv) t
    return (s2, apply s1 tv)
  Op op e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    tv <- fresh
    s3 <- unify (TArr t1 (TArr t2 tv)) (ops Map.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)
  Lit (LInt _) -> return (emptySubst, typeInt)
  Lit (LBool _) -> return (emptySubst, typeBool)

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

ops :: Map BinOp Type
ops =
  Map.fromList
    [ (Add, typeInt `TArr` (typeInt `TArr` typeInt)),
      (Mul, typeInt `TArr` (typeInt `TArr` typeInt)),
      (Sub, typeInt `TArr` (typeInt `TArr` typeInt)),
      (Eql, typeInt `TArr` (typeInt `TArr` typeBool))
    ]

lookupEnv :: InferM m => TypeEnv -> Name -> m (Subst, Type)
lookupEnv (TypeEnv env) x = do
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> do
      t <- instantiate s
      return (emptySubst, t)
