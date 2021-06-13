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
  pp (Lam x e) = "\\" <> (pretty x) <+> annParens "->" <+> pp e
  pp (App e1 e2) = pp e1 <+> " " <+> pp e2
    where
      undo = unAnnotate
  pp (Lit l) = pp l

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

instance PP Lit where
  pp (LInt i) = pretty i
  pp (LBool b) = pretty b

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
