module Poly.Syntax
  ( Expr (..),
    Lit (..),
    Name,
    BinOp (..),
    Program (..),
    Decl (..),
  )
where

import Data.Text (Text)
import Poly.Pretty
import Prettyprinter

type Name = Text

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op BinOp Expr Expr
  deriving (Show, Eq, Ord)

instance PP Expr where
  pp (Var x) = pretty x
  pp (App e1 e2) = annNest (pp e1) <+> pp e2
  pp (Lam x e) = annParensIf $ "\\" <> pretty x <+> "->" <+> pp e
  pp (Let a b c) = "let" <+> pretty a <+> "=" <+> pp b <+> "in" <+> pp c
  pp (Lit l) = pp l
  pp (Op o a b) = annParensIf $ pp a <+> pp o <+> pp b
  pp (Fix a) = annParensIf $ "fix" <+> pp a
  pp (If a b c) = "if" <+> pp a <+> "then" <+> pp b <+> "else" <+> pp c

data Lit
  = LInt Integer
  | LBool Bool
  | LStr Text
  | LChar Char
  deriving (Show, Eq, Ord)

instance PP Lit where
  pp (LInt i) = pretty i
  pp (LBool b) = pretty b
  pp (LStr s) = pretty s
  pp (LChar c) = pretty c

data BinOp = Add | Sub | Mul | Div | Eql | Neql
  deriving (Show, Eq, Ord)

instance PP BinOp where
  pp Add = "+"
  pp Sub = "-"
  pp Mul = "*"
  pp Div = "/"
  pp Eql = "=="
  pp Neql = "!="

data Decl = Decl Text Expr
  deriving (Show, Eq)

data Program = Program [Decl] Expr deriving (Show, Eq)
