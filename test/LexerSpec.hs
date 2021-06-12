module LexerSpec  where

import Text.Megaparsec
import Poly.Lexer
import Test.Hspec

-- lex :: Parser a -> a
-- lex = parseTest

spec :: Spec
spec = parallel $ do
  describe "lexer" $ do
    it "" $ do
      1 `shouldBe` 1
