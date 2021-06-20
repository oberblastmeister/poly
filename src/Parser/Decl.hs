module Parser.Decl
  ( decl,
    top,
    modl,
    prog,
  )
where

import AST.Decl
import AST.Expr
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Name
import Parser.Expr
import Parser.Lexer
import Parser.Primitives
import Parser.Type (pType)
import Text.Megaparsec
import Type.Types

letDecl :: Parser Decl
letDecl = do
  reserved "let"
  name <- ident
  args <- many ident
  symbol "="
  body <- expr
  return $ DStmt name (foldr Lam body args)

letRecDecl :: Parser Decl
letRecDecl = do
  reserved "let"
  reserved "rec"
  name <- ident
  args <- many ident
  symbol "="
  body <- expr
  return $ DStmt name (Fix $ foldr Lam body (name : args))

typeDecl :: Parser Decl
typeDecl = do
  reserved "type"
  name <- ident
  symbol "="
  body <- adtBody
  return $ DType name body

adtBody :: Parser ADTBody
adtBody =
  Record <$> recordBody
    <|> Enum <$> enumBody

enumBody :: Parser [(Name, Type)]
enumBody = some variant

recordBody :: Parser [(Name, Type)]
recordBody = brackets $ some field

field :: Parser (Name, Type)
field = (,) <$> ident <* symbol ":" <*> pType

variant :: Parser (Name, Type)
variant = (,) <$> (symbol "|" *> ident) <* reserved "of" <*> pType

val :: Parser Decl
val = DExpr <$> expr

decl :: Parser Decl
decl =
  choice @[]
    [ typeDecl,
      try letRecDecl,
      letDecl,
      val
    ]

top :: Parser Decl
top = decl <* optional semi

modl :: Parser [Decl]
modl = many top

prog :: Parser Program
prog = Program <$> many top <*> optional expr
