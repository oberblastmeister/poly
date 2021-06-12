module Poly.Syntax
  ( Expr (..),
    Lit (..),
    Name,
    BinOp (..),
    Program (..),
    Decl,
  )
where

import Data.Text (Text)

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

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data BinOp = Add | Sub | Mul | Div | Eql | Neql
  deriving (Show, Eq, Ord)

type Decl = (Text, Expr)

data Program = Program [Decl] Expr deriving (Eq)
