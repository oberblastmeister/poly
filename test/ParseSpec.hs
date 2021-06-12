module ParseSpec (spec) where

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
          parseTest "True" `shouldBe` Lit (LBool True)
          parseTest "False" `shouldBe` Lit (LBool False)

        it "should parse integer" $ do
          parseTest "34123" `shouldBe` Lit (LInt 34123)

      it "should parse if then" $ do
        parseTest "if True then 12 else 234"
          `shouldBe` If
            (Lit $ LBool True)
            (Lit $ LInt 12)
            (Lit $ LInt 234)

      describe "lambda" $ do
        it "should parse lambda" $ do
          parseTest "\\a -> a" `shouldBe` Lam "a" (Var "a")

        it "should parse multi lambda" $ do
          parseTest "\\a b -> b a" `shouldBe` Lam "a" (Lam "b" (App (Var "b") (Var "a")))

      describe "binary" $ do
        it "should parse simple" $ do
          parseTest "1234 == 1234" `shouldBe` Op Eql (Lit $ LInt 1234) (Lit $ LInt 1234)
          parseTest "1 - 1" `shouldBe` Op Sub (Lit $ LInt 1) (Lit $ LInt 1)
          parseTest "12 + 12" `shouldBe` Op Add (Lit $ LInt 12) (Lit $ LInt 12)
          parseTest "0 * 0 " `shouldBe` Op Mul (Lit $ LInt 0) (Lit $ LInt 0)
          parseTest "123 / 123" `shouldBe` Op Div (Lit $ LInt 123) (Lit $ LInt 123)
          parseTest "True != False" `shouldBe` Op Neql (Lit $ LBool True) (Lit $ LBool False)

        it "should parse precedence correctly" $ do
          parseTest "34 + 234 * 123"
            `shouldBe` Op
              Add
              (Lit $ LInt 34)
              ( Op
                  Mul
                  (Lit $ LInt 234)
                  (Lit $ LInt 123)
              )

        it "should parse parens" $ do
          parseTest "(34 + 234) * 123"
            `shouldBe` Op
              Mul
              ( Op
                  Add
                  (Lit $ LInt 34)
                  (Lit $ LInt 234)
              )
              (Lit $ LInt 123)

    describe "modules" $ do
      it "" $ do
        1 `shouldBe` 1
