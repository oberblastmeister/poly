module Type.Env
  ( Env (..),
    empty,
    lookup,
    extend,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Name (Name)
import Type.Types
import Prelude hiding (lookup)

newtype Env = Env {types :: Map Name Scheme}
  deriving (Show)

empty :: Env
empty = Env Map.empty

lookup :: Name -> Env -> Maybe Scheme
lookup x (Env env) = Map.lookup x env

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env {types = Map.insert x s (types env)}
