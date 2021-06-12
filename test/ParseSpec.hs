module ParseSpec (spec) where

import Data.Either.Combinators
import Data.Function
import Data.Text (Text)
import Poly.Parser
import Poly.Syntax
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

parseTest :: Text -> Expr
parseTest s = parseExpr s & either (error . errorBundlePretty) id

spec :: Spec
spec = parallel $ do
  describe "parser" $ do
    describe "expressions" $ do
      describe "literals" $ do
        it "should parse bool" $ do
          parseExpr "True" `shouldBe` Right (Lit $ LBool True)
          parseExpr "False" `shouldBe` Right (Lit $ LBool False)

        it "should parse integer" $ do
          parseExpr "34123" `shouldBe` Right (Lit $ LInt 34123)

      it "should parse if then" $ do
        parseTest "if True then 12 else 234"
          `shouldBe` If
            (Lit $ LBool True)
            (Lit $ LInt 12)
            (Lit $ LInt 234)

    describe "modules" $ do
      it "" $ do
        1 `shouldBe` 1
