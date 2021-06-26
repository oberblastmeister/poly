module Type.Unify where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Set as Set
import Type.InferMonad
import Type.Types
import Prelude hiding (lookup)

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
