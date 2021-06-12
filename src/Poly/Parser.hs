module Poly.Parser (parseExpr, parseModule) where

import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text (Text)
import Data.Void
import Poly.Lexer
import Poly.Syntax
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)

variable :: Parser Expr
variable = Var <$> ident

number :: Parser Expr
number = Lit . LInt <$> integer

bool :: Parser Expr
bool = reserved "True" $> b True <|> reserved "False" $> b False
  where
    b = Lit . LBool

fix :: Parser Expr
fix = lexeme "fix" *> expr <&> Fix

lambda :: Parser Expr
lambda = do
  symbol "\\"
  args <- many ident
  symbol "->"
  body <- expr
  return $ foldr Lam body args

letIn :: Parser Expr
letIn = do
  reserved "let"
  x <- ident
  symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ Let x e1 e2

letRecIn :: Parser Expr
letRecIn = do
  reserved "let"
  reserved "rec"
  x <- ident
  symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ Let x e1 e2

ifThen :: Parser Expr
ifThen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

atomExpr :: Parser Expr
atomExpr =
  choice
    [ parens expr,
      bool,
      number,
      ifThen,
      fix,
      try letRecIn,
      letIn,
      lambda,
      variable
    ]

term :: Parser Expr
-- term = atomExpr
term =
  atomExpr >>= \x ->
    (some atomExpr >>= \xs -> return (foldl App x xs))
      <|> return x

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "*" Mul
    ],
    [ binary "+" Add,
      binary "-" Sub
    ],
    [ binary "==" Eql
    ]
  ]

binary :: Text -> BinOp -> Operator Parser Expr
binary name binOp = InfixL (Op binOp <$ symbol name)

expr :: Parser Expr
expr = makeExprParser term operatorTable

type Binding = (Name, Expr)

letDecl :: Parser Binding
letDecl = do
  reserved "let"
  name <- ident
  args <- many ident
  symbol "="
  body <- expr
  return (name, foldr Lam body args)

letRecDecl :: Parser Binding
letRecDecl = do
  reserved "let"
  reserved "rec"
  name <- ident
  args <- many ident
  symbol "="
  body <- expr
  return (name, Fix $ foldr Lam body (name : args))

val :: Parser Binding
val = ("it",) <$> expr

decl :: Parser Binding
decl = try letRecDecl <|> letDecl <|> val

top :: Parser Binding
top = decl <* optional semi

modl :: Parser [Binding]
modl = many top

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule :: Text -> Either (ParseErrorBundle Text Void) [Binding]
parseModule = parse (contents modl) "<stdin>"
