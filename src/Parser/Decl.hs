module Parser.Decl
  ( decl,
    top,
    modl,
    prog,
    parseDecl,
  )
where

import AST.Decl
import qualified AST.Expr as Expr
import Data.Name
import Data.Text (Text)
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
  return $ DStmt name (foldr Expr.Lam body args)

letRecDecl :: Parser Decl
letRecDecl = do
  reserved "let"
  reserved "rec"
  name <- ident
  args <- many ident
  symbol "="
  body <- expr
  return $ DStmt name (Expr.Fix $ foldr Expr.Lam body (name : args))

typeDecl :: Parser Decl
typeDecl = do
  reserved "type"
  name <- pascalIdent
  symbol "="
  body <- adtBody
  return $ DType name body

adtBody :: Parser ADTBody
adtBody =
  Record <$> recordBody
    <|> Enum <$> enumBody

enumBody :: Parser [(Name, Maybe Type)]
enumBody = some variant

recordBody :: Parser [(Name, Type)]
recordBody = braces $ sepEndBy1 field semi

field :: Parser (Name, Type)
field = (,) <$> ident <* symbol ":" <*> pType

variant :: Parser (Name, Maybe Type)
variant = (,) <$> (symbol "|" *> pascalIdent) <*> optional variantType

variantType :: Parser Type
variantType = reserved "of" *> pType

typeSynonymDecl :: Parser Decl
typeSynonymDecl = DSynonym <$> (reserved "type" *> pascalIdent <* symbol "=") <*> pType

val :: Parser Decl
val = DExpr <$> expr

decl :: Parser Decl
decl =
  choice @[]
    [ try typeDecl <?> "type declaration",
      typeSynonymDecl <?> "type synonym",
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

parseDecl :: Text -> Either PError Decl
parseDecl = parseFull decl
