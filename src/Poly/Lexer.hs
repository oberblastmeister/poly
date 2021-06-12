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
parens = between (char '(') (char ')')

ident :: Parser Text
ident = do
  c <- letterChar
  cs <- takeWhileP (Just "identifier continue") isIdentContinue
  return (c `T.cons` cs)
  where
    isIdentContinue = (||) <$> isAlphaNum <*> (== '_')

reserved :: Text -> Parser ()
reserved keyword = lexeme (string keyword >> notFollowedBy alphaNumChar)

contents :: Parser a -> Parser a
contents p = sc *> p <* eof

integer :: Parser Integer
integer = lexeme L.decimal

semi :: Parser ()
semi = () <$ char ';'
