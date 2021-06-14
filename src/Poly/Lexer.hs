module Poly.Lexer where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

mkReserved :: [Text] -> Parser ()
mkReserved l = choice $ (() <$) . symbol <$> l

reservedKeywords :: Parser ()
reservedKeywords =
  mkReserved
    [ "let",
      "in",
      "fix",
      "rec",
      "if",
      "then",
      "else"
    ]

ident :: Parser Text
ident = lexeme ident'
  where
    ident' = do
      notFollowedBy reservedKeywords
      c <- letterChar
      cs <- takeWhileP (Just "identifier") isIdentContinue
      return (c `T.cons` cs)
    isIdentContinue = (||) <$> isAlphaNum <*> (== '_')

reserved :: Text -> Parser ()
reserved keyword =
  lexeme (string keyword *> notFollowedBy alphaNumChar)

contents :: Parser a -> Parser a
contents p = sc *> p <* eof

integer :: Parser Integer
integer = lexeme L.decimal

strTok :: Parser Text
strTok = T.pack <$> res
  where
    res = char '\"' *> manyTill L.charLiteral (char '\"')

charTok :: Parser Char
charTok = between (char '\'') (char '\'') L.charLiteral

semi :: Parser ()
semi = () <$ symbol ";"
