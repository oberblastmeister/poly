module Poly.Type
  ( TVar (..),
    Type (..),
    TCon (..),
    intBinFun,
    Scheme (..),
  )
where

import Data.Text (Text)
import Poly.Pretty
import Prettyprinter
import TextShow

data Scheme = Forall [TVar] Type deriving (Show)

instance PP Scheme where
  pp (Forall [] t) = pp t
  pp (Forall ts t) = "forall" <+> hcat (punctuate space $ pretty <$> ts) <+> "." <+> pp t

newtype TVar = TV Text
  deriving (Show, TextShow, Eq, Ord, Pretty)

instance PP TVar where
  pp (TV t) = pretty t

data Type
  = TVar TVar
  | TCon TCon
  | TArr Type Type
  deriving (Show, Eq)

instance PP Type where
  pp (TArr t1 t2) = shouldParens (isArr t1) (annNest $ pp t1) <+> "->" <+> pp t2
    where
      shouldParens True = annParens
      shouldParens False = id
      isArr TArr {} = True
      isArr _ = False
  pp (TVar v) = pretty v
  pp (TCon t) = pp t

data TCon
  = TInt
  | TBool
  deriving (Show, Eq)

instance PP TCon where
  pp TInt = "Int"
  pp TBool = "Bool"

intBinFun :: Type
intBinFun = TCon TInt `TArr` (TCon TInt `TArr` TCon TInt)
