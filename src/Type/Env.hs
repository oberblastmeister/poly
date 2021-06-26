module Type.Env where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Name (Name)
import Data.Text (Text)
import Lens.Micro
import Lens.Micro.TH
import Type.Types
import Prelude hiding (lookup)

data Env = Env
  { _names :: Map Name Scheme,
    _adts :: Map Name (),
    _tvars :: Map Name TVar
  }
  deriving (Show)

makeLenses ''Env

empty :: Env
empty =
  Env
    { _names = Map.empty,
      _adts = Map.empty,
      _tvars = []
    }

lookupScheme :: Name -> Env -> Maybe Scheme
lookupScheme x env = Map.lookup x (env ^. names)

addScheme :: Env -> (Name, Scheme) -> Env
-- addScheme env (x, s) = env {_names = Map.insert x s (env ^. _names)}
addScheme env (x, s) = env & names %~ Map.insert x s
