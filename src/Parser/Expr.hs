module Parser.Expr
  ( parseExpr,
    expr,
  )
where

import AST.Expr
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text (Text)
import Debug.Trace (trace)
import Parser.Lexer
  ( charTok,
    ident,
    integer,
    parens,
    reserved,
    strTok,
  )
import Parser.Primitives
import Parser.Type (pType)
import Text.Megaparsec

variable :: Parser Expr
variable = Var <$> ident

number :: Parser Expr
number = Lit . LInt <$> integer

bool :: Parser Expr
bool = reserved "True" $> b True <|> reserved "False" $> b False
  where
    b = Lit . LBool

str :: Parser Expr
str = Lit . LStr <$> strTok

ch :: Parser Expr
ch = Lit . LChar <$> charTok

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
  name <- ident
  symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  -- return $ Let name e1 e2
  return $ Let name (Fix $ Lam name e1) e2
  -- return $ Let name (Fix $ foldr @[] Lam e1 [name]) e2

ifThen :: Parser Expr
ifThen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

typed :: Parser Expr
typed = parens (Typed <$> (expr <* symbol ":") <*> pType)

atomExpr :: Parser Expr
atomExpr =
  choice @[]
    [ try $ parens expr,
      typed,
      bool,
      number,
      str,
      ch,
      ifThen,
      fix,
      try letRecIn,
      letIn,
      lambda,
      variable
    ]

term :: Parser Expr
term =
  atomExpr >>= \x ->
    (some atomExpr >>= \xs -> return (foldl App x xs))
      <|> return x

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ bin "*" Mul,
      bin "/" Div
    ],
    [ bin "+" Add,
      bin "-" Sub
    ],
    [ bin "==" Eql,
      bin "!=" Neql
    ]
  ]
  where
    bin t op = infixL t (Bin op)

expr :: Parser Expr
expr = makeExprParser term operatorTable

parseExpr :: Text -> Either PError Expr
parseExpr = parseFull expr
