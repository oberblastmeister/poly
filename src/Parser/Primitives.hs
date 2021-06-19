module Parser.Primitives
  ( lexeme,
    Parser,
    symbol,
    infixL,
    infixR,
    contents,
    parseFull,
    enclosedBy,
    PError (..),
  )
where

import Control.Monad.Combinators.Expr
import Data.Either.Combinators
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import TextShow

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

infixL, infixR :: Text -> (t -> t -> t) -> Operator Parser t
infixL name binExpr = InfixL (binExpr <$ symbol name)
infixR name binExpr = InfixR (binExpr <$ symbol name)

contents :: Parser a -> Parser a
contents p = sc *> p <* eof

parseFull :: Parser a -> Text -> Either PError a
parseFull p s = mapLeft PError $ parse (contents p) "<stdin>" s

enclosedBy :: Text -> Text -> Parser a -> Parser a
enclosedBy s s' = between (symbol s) (symbol s')

newtype PError = PError (ParseErrorBundle Text Void)
  deriving (Eq)

instance Show PError where
  show (PError e) = errorBundlePretty e

instance TextShow PError where
  showb e = fromString $ show e
