module Util where

unwrap :: Show a => Either a b -> b
unwrap = either (error . show) id
