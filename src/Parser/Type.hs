module Parser.Type
  ( parseType,
    pType,
  )
where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Parser.Lexer
import Parser.Primitives
import Text.Megaparsec
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
tyVar = TVar . TV <$> ident

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
