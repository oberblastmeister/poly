module Parser.Decl
  ( decl,
    top,
    modl,
    prog
  )
  where

import AST
import Parser.Expr
import Parser.Lexer
import Parser.Primitives
import Parser.Type (pType)
import Text.Megaparsec

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
adtBody = Record <$> recordBody <|> Enum <$> enumBody

enumBody :: Parser [Variant]
enumBody = some variant

recordBody :: Parser [Field]
recordBody = brackets $ some field

field :: Parser Field
field = Field <$> ident <* symbol ":" <*> pType

variant :: Parser Variant
variant = Variant <$> (symbol "|" *> ident) <* reserved "of" <*> pType

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
