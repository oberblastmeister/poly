module AST.Decl
  ( Decl (..),
    Program (..),
    ADTBody (..),
  )
where

import AST.Expr
import Data.Data (Data, Typeable)
import Data.Name
import Data.Text (Text)
import Poly.Pretty
import Prettyprinter
import Test.QuickCheck.Instances ()
import TextShow
import Type.Types

data Decl
  = DStmt Text Expr
  | DExpr Expr
  | DType Name ADTBody
  | DSynonym Name Type
  deriving (Eq, Data, Typeable)
  deriving (Show) via PPShow Decl
  deriving (TextShow) via PPShow Decl

instance PP Decl where
  pp (DStmt x e) = "let" <+> pretty x <+> "=" <+> pp e
  pp (DExpr e) = pp e <> ";"

data Program = Program [Decl] (Maybe Expr)
  deriving (Show, Eq, Data, Typeable)

data ADTBody
  = Record [(Name, Type)]
  | Enum [(Name, Maybe Type)]
  deriving (Eq, Data, Typeable)
