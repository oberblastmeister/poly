module ParseSpec (spec) where

import Data.Either.Combinators
import Data.Function
import Data.Text (Text)
import Poly.Parser
import Poly.Syntax
import Test.Hspec

unwrap :: Either String r -> r
unwrap = either error id

parseExprTest :: Text -> Expr
parseExprTest s = parseExpr s & mapLeft show & unwrap

parseModTest :: Text -> [Decl]
parseModTest s = parseModule s & mapLeft show & unwrap

parseProgTest :: Text -> Program
parseProgTest s = parseProgram s & mapLeft show & unwrap

spec :: Spec
spec = parallel $ do
  describe "expressions" $ do
    describe "literals" $ do
      it "should parse bool" $ do
        parseExprTest "True" `shouldBe` Lit (LBool True)
        parseExprTest "False" `shouldBe` Lit (LBool False)

      it "should parse integer" $ do
        parseExprTest "34123" `shouldBe` Lit (LInt 34123)

    describe "lambda" $ do
      it "should parse lambda" $ do
        parseExprTest "\\a -> a" `shouldBe` Lam "a" (Var "a")

      it "should parse multi lambda" $ do
        parseExprTest "\\a b -> b a" `shouldBe` Lam "a" (Lam "b" (App (Var "b") (Var "a")))

    -- it "should parse app" $ do
    --   parseExprTest "\\a -> a 234" `shouldBe` App (Lam "a" (Var "a")) (Lit $ LInt 234)

    describe "binary" $ do
      it "should parse simple" $ do
        parseExprTest "1234 == 1234" `shouldBe` Op Eql (Lit $ LInt 1234) (Lit $ LInt 1234)
        parseExprTest "1 - 1" `shouldBe` Op Sub (Lit $ LInt 1) (Lit $ LInt 1)
        parseExprTest "12 + 12" `shouldBe` Op Add (Lit $ LInt 12) (Lit $ LInt 12)
        parseExprTest "0 * 0 " `shouldBe` Op Mul (Lit $ LInt 0) (Lit $ LInt 0)
        parseExprTest "123 / 123" `shouldBe` Op Div (Lit $ LInt 123) (Lit $ LInt 123)
        parseExprTest "True != False" `shouldBe` Op Neql (Lit $ LBool True) (Lit $ LBool False)

      it "should parse precedence correctly" $ do
        parseExprTest "34 + 234 * 123"
          `shouldBe` Op
            Add
            (Lit $ LInt 34)
            ( Op
                Mul
                (Lit $ LInt 234)
                (Lit $ LInt 123)
            )

      it "should parse parens" $ do
        parseExprTest "(34 + 234) * 123"
          `shouldBe` Op
            Mul
            ( Op
                Add
                (Lit $ LInt 34)
                (Lit $ LInt 234)
            )
            (Lit $ LInt 123)

    it "should parse if then" $ do
      parseExprTest "if True then 12 else 234"
        `shouldBe` If
          (Lit $ LBool True)
          (Lit $ LInt 12)
          (Lit $ LInt 234)

    it "should parse let" $ do
      parseExprTest "let rec x = 324 in x" `shouldBe` Let "x" (Lit $ LInt 324) (Var "x")
      parseExprTest "let x = 324 in x" `shouldBe` Let "x" (Lit $ LInt 324) (Var "x")
      parseExprTest "let x = 324 in let y = 213 in x"
        `shouldBe` Let
          "x"
          (Lit $ LInt 324)
          (Let "y" (Lit $ LInt 213) (Var "x"))

    it "should parse fix" $ do
      parseExprTest "fix 123" `shouldBe` Fix (Lit $ LInt 123)
      parseExprTest "fix let x = True in x"
        `shouldBe` Fix
          ( Let
              "x"
              (Lit $ LBool True)
              (Var "x")
          )
  -- describe "program" $ do
  --   it "should parse single ret" $ do
  --     parseProgTest "1324" `shouldBe` Program [] (Lit $ LInt 1324)
  -- it "should parse expr decls then ret" $ do
  --   parseProgTest "1; 1; 1; 1" `shouldBe` Program (replicate 3 (DeclExpr $ Lit $ LInt 1)) (Lit $ LInt 1)

  describe "modules" $ do
    it "should parse val" $ do
      parseModTest "1234" `shouldBe` [DeclExpr (Lit $ LInt 1234)]
      parseModTest "True" `shouldBe` [DeclExpr (Lit $ LBool True)]
      parseModTest "True; False" `shouldBe` [DeclExpr (Lit $ LBool True), DeclExpr (Lit $ LBool False)]

    it "should parse let decl" $ do
      parseModTest "let x = 1234;" `shouldBe` [Decl "x" (Lit $ LInt 1234)]
      parseModTest "let rec x y = x y;" `shouldBe` [Decl "x" (Fix $ Lam "x" $ Lam "y" $ App (Var "x") (Var "y"))]
