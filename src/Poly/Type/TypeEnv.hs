module Poly.Type.TypeEnv where

import Data.Map (Map)
import qualified Data.Map as Map
import Poly.Syntax
import Poly.Type.Types

newtype TypeEnv = TypeEnv {types :: Map Name Scheme}
  deriving (Show)

empty :: TypeEnv
empty = TypeEnv Map.empty

lookup :: Name -> TypeEnv -> Maybe Scheme
lookup x (TypeEnv env) = Map.lookup x env

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend env (x, s) = env {types = Map.insert x s (types env)}
