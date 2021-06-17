{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Supply
  ( evalSupplyT,
    evalSupply,
    runSupplyT,
    runSupply,
    MonadSupply (..),
    Supply,
    SupplyT,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (..), MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.State (StateT (runStateT), evalStateT, get, gets, put)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Writer (WriterT (WriterT))

newtype SupplyT s m a = SupplyT {unSupplyT :: StateT [s] m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

newtype Supply s a = Supply (SupplyT s Maybe a)
  deriving (Functor, Applicative, Monad, MonadSupply s, MonadFix)

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> [s] -> Maybe a
evalSupply (Supply s) = evalSupplyT s

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m (a, [s])
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> [s] -> Maybe (a, [s])
runSupply (Supply s) = runSupplyT s

instance MonadError e m => MonadError e (SupplyT s m) where
  throwError = lift . throwError

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  isExhausted :: m Bool

instance MonadFail m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    (x : xs) <- get
    put xs
    return x
  isExhausted = SupplyT $ gets null

instance MonadSupply s m => MonadSupply s (ExceptT e m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (MaybeT m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply
  isExhausted = lift isExhausted
