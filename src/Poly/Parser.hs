module Poly.Parser where

import Control.Monad.Combinators.Expr
import Data.Either.Combinators
import Data.Functor
import Data.Text (Text)
import Data.Void
import Poly.Lexer
import Poly.Syntax
import Poly.Type
import Text.Megaparsec
import TextShow

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
  choice @[]
    [ parens expr,
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

infixL, infixR :: Text -> (t -> t -> t) -> Operator Parser t
infixL name binExpr = InfixL (binExpr <$ symbol name)
infixR name binExpr = InfixR (binExpr <$ symbol name)

expr :: Parser Expr
expr = makeExprParser term operatorTable

tyLit :: Parser Type
tyLit =
  TCon
    <$> choice @[]
      [ TBool <$ reserved "Bool",
        TInt <$ reserved "Int",
        TChar <$ reserved "Char",
        TStr <$ reserved "Str"
      ]

tyAtom :: Parser Type
tyAtom =
  choice @[]
    [ parens pType,
      tyLit
    ]

tyOps :: [[Operator Parser Type]]
tyOps =
  [ [ infixR "->" (:->:)
    ]
  ]

pType :: Parser Type
pType = makeExprParser tyAtom tyOps

letDecl :: Parser Decl
letDecl = do
  reserved "let"
  name <- ident
  args <- many ident
  symbol "="
  body <- expr
  return $ Decl name (foldr Lam body args)

letRecDecl :: Parser Decl
letRecDecl = do
  reserved "let"
  reserved "rec"
  name <- ident
  args <- many ident
  symbol "="
  body <- expr
  return $ Decl name (Fix $ foldr Lam body (name : args))

val :: Parser Decl
val = DeclExpr <$> expr

decl :: Parser Decl
decl = try letRecDecl <|> letDecl <|> val

top :: Parser Decl
top = decl <* optional semi

-- modRet :: Parser Expr
-- modRet = expr

modl :: Parser [Decl]
modl = many top

prog :: Parser Program
prog = Program <$> many top <*> optional expr

newtype PError = PError (ParseErrorBundle Text Void)
  deriving (Eq)

instance Show PError where
  show (PError e) = errorBundlePretty e

instance TextShow PError where
  showb e = fromString $ show e

parseFull :: Parser a -> Text -> Either PError a
parseFull p s = mapLeft PError $ parse (contents p) "<stdin>" s

parseExpr :: Text -> Either PError Expr
parseExpr = parseFull expr

parseModule :: Text -> Either PError [Decl]
parseModule = parseFull modl

parseProgram :: Text -> Either PError Program
parseProgram = parseFull prog

parseType :: Text -> Either PError Type
parseType = parseFull pType
