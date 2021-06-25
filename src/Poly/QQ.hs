module Poly.QQ
  ( ex,
    ty,
    pty,
    tv,
  )
where

import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (cast)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Parser.Expr (parseExpr)
import Parser.Type (parseScheme, parseType, parseTVar)

defaultQQ :: QuasiQuoter
defaultQQ =
  QuasiQuoter
    { quoteExp = error "Usage as an expression is not supported",
      quotePat = error "Usage as a pattern is not supported",
      quoteType = error "Usage as a type is not supported",
      quoteDec = error "Usage as a declaration is not supported"
    }

liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftData' :: Data a => a -> Q Exp
liftData' = dataToExpQ (fmap liftText . cast)

parseLift :: (Show a, Data b) => (Text -> Either a b) -> String -> Q Exp
parseLift p s = case p $ T.pack s of
  Left e -> fail $ show e
  Right res -> do
    liftData' res

quoteExprExp :: String -> Q Exp
quoteExprExp = parseLift parseExpr

quoteTypeExp :: String -> Q Exp
quoteTypeExp = parseLift parseType

quotePTypeExp :: String -> Q Exp
quotePTypeExp = parseLift parseScheme

quoteTVarExp :: String -> Q Exp
quoteTVarExp = parseLift parseTVar

ex :: QuasiQuoter
ex = defaultQQ {quoteExp = quoteExprExp}

ty :: QuasiQuoter
ty = defaultQQ {quoteExp = quoteTypeExp}

pty :: QuasiQuoter
pty = defaultQQ {quoteExp = quotePTypeExp}

tv :: QuasiQuoter
tv = defaultQQ {quoteExp = quoteTVarExp}

-- pty :: QuasiQuoter
-- pty = defaultQQ {quoteExp = quoteSchemeExp}
