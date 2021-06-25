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
import Parser.Type (parseType)
import Type.Env (empty)
import Type.Infer (generalize)
import Type.Types (TVar (..), Type (TVar))

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

quoteExprExp :: String -> Q Exp
quoteExprExp s = do
  case parseExpr $ T.pack s of
    Left e -> fail $ show e
    Right expr -> do
      liftData' expr

quoteTypeExp :: String -> Q Exp
quoteTypeExp s = do
  case parseType $ T.pack s of
    Left e -> fail $ show e
    Right t -> do
      liftData' t

quotePTypeExp :: String -> Q Exp
quotePTypeExp s = do
  case parseType $ T.pack s of
    Left e -> fail $ show e
    Right t -> do
      liftData' $ generalize empty t

quoteTVarExp :: String -> Q Exp
quoteTVarExp s = do
  case parseType $ T.pack s of
    Left e -> fail $ show e
    Right t -> do
      let TVar v = t
      liftData' v

-- quoteSchemeExp :: String -> Q Exp
-- quoteSchemeExp s = do
--   case parseScheme $ T.pack s of
--     Left e -> fail $ show e
--     Right t -> do
--       liftData' t

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
