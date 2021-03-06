module LexerSpec (spec) where

import Data.Maybe
import Data.Text (Text)
import Parser.Lexer
import Parser.Primitives
import Test.Hspec
import Text.Megaparsec
import Prelude hiding (lex)

lex :: Parser a -> Text -> Maybe a
lex l = parseMaybe (contents l)

spec :: Spec
spec = parallel $ do
  it "should lex valid idents properly" $ do
    isJust . lex ident
      <$> [ "hello",
            "another",
            "rec",
            "let"
          ]
      `shouldMatchList` [ True,
                          True,
                          False,
                          False
                        ]

-- lex ident "hello" `shouldSatisfy` isJust
