module ParseSpec (spec) where

import Test.Hspec
import Poly.Parser

spec :: Spec
spec = parallel $ do
  describe "parser" $ do
    it "should parse literals" $ do
      1 `shouldBe` 1
