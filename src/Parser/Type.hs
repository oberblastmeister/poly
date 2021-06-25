module Parser.Type
  ( parseType,
    pType,
    parseScheme,
    parseTVar
  )
where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Parser.Lexer
import Parser.Primitives
import Text.Megaparsec hiding (empty)
import Type.Env (empty)
import Type.Infer (generalize)
import Type.Types

tyLit :: Parser Type
tyLit =
  TCon
    <$> choice @[]
      [ TBool <$ reserved "Bool",
        TInt <$ reserved "Int",
        TChar <$ reserved "Char",
        TStr <$ reserved "Str"
      ]

tyVar :: Parser Type
tyVar = TVar . TVNamed <$> ident

tyADT :: Parser Type
tyADT = ADTTCon <$> pascalIdent

tyAtom :: Parser Type
tyAtom =
  choice @[]
    [ parens pType,
      tyLit,
      tyADT,
      tyVar
    ]

tyOps :: [[Operator Parser Type]]
tyOps =
  [ [ infixR "->" (:->:)
    ]
  ]

pType :: Parser Type
pType = makeExprParser tyAtom tyOps

parseType :: Text -> Either PError Type
parseType = parseFull pType

parseScheme :: Text -> Either PError Scheme
parseScheme s = do
  res <- parseType s
  return $ generalize empty res

parseTVar :: Text -> Either PError TVar
parseTVar s = do
  ty <- parseType s
  let TVar v = ty
  return v
