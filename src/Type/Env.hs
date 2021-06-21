module Type.Env where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Name (Name)
import Lens.Micro
import Lens.Micro.TH
import Type.Types
import Prelude hiding (lookup)

data Env = Env {_types :: Map Name Scheme, _adts :: Map Name ()}
  deriving (Show)

makeLenses ''Env

empty :: Env
empty = Env {_types = Map.empty, _adts = Map.empty}

lookupScheme :: Name -> Env -> Maybe Scheme
lookupScheme x env = Map.lookup x (env ^. types)

addScheme :: Env -> (Name, Scheme) -> Env
addScheme env (x, s) = env {_types = Map.insert x s (env ^. types)}
