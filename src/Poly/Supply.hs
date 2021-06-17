module Poly.Supply  where

newtype TVar = TVarID Int

newSupply :: [TVar]
newSupply = TVarID <$> [1 ..]
