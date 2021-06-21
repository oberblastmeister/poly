module PrettySpec (spec) where

import AST.Expr
import Data.Text (Text)
import Poly.Pretty
import Poly.QQ
import Test.Hspec
import Type.Types

check :: PP p => p -> Text -> Expectation
check ty s = ppr ty `shouldBe` s

checks :: PP p => [(p, Text)] -> Expectation
checks list = mapM_ (\(ty, s) -> check ty s) list

spec :: Spec
spec = parallel $ do
  describe "types" $ do
    it "should pretty print TVar" $ do
      checks
        [ ([ty|asdf|], "asdf"),
          ([ty|a|], "a"),
          ([ty|(a)|], "a")
        ]
    it "should pretty print arrows" $ do
      checks
        [ ([ty|a -> a -> b|], "a -> a -> b"),
          ([ty|a -> b -> c -> d|], "a -> b -> c -> d"),
          ([ty|a -> Str|], "a -> Str"),
          ([ty|Str -> Str -> Bool|], "Str -> Str -> Bool"),
          ([ty|(Str -> Str) -> Bool|], "(Str -> Str) -> Bool"),
          ([ty|Str -> Bool -> (Str -> Int)|], "Str -> Bool -> Str -> Int"),
          ([ty|Str -> (Bool -> Str) -> Int|], "Str -> (Bool -> Str) -> Int")
        ]

  describe "expressions" $ do
    it "should pretty print lam" $ do
      checks
        [ ([ex|\x -> x|], "\\x -> x"),
          ([ex|\x y -> y x|], "\\x -> \\y -> y x"),
          ([ex|x y z w g|], "x y z w g")
        ]
