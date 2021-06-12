module Poly.Type where

import Data.Text (Text)

newtype TVar = TV Text
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon Text
  | TArr Type Type
  deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt = TCon "Int"
typeBool = TCon "Bool"
