module Parser.Lexer
  ( parens,
    braces,
    brackets,
    ident,
    pascalIdent,
    reserved,
    integer,
    strTok,
    charTok,
    semi,
  )
  where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Primitives
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parens :: Parser a -> Parser a
parens = enclosedBy "(" ")"

braces :: Parser a -> Parser a
braces = enclosedBy "{" "}"

brackets :: Parser a -> Parser a
brackets = enclosedBy "[" "]"

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
      "else",
      "of"
    ]

ident :: Parser Text
ident = lexeme ident'
  where
    ident' = do
      notFollowedBy reservedKeywords
      c <- letterChar
      cs <- takeWhileP (Just "identifier") isIdentContinue
      return $ c `T.cons` cs

isIdentContinue :: Char -> Bool
isIdentContinue = (||) <$> isAlphaNum <*> (== '_')

pascalIdent :: Parser Text
pascalIdent = lexeme $ do
  c <- upperChar
  cs <- takeWhileP (Just "identitifer") isIdentContinue
  return $ c `T.cons` cs

reserved :: Text -> Parser ()
reserved keyword =
  lexeme (string keyword *> notFollowedBy alphaNumChar)

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
