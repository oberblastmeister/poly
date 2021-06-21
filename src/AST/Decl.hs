module AST.Decl
  ( Decl (..),
    Program (..),
    ADTBody (..),
  )
where

-- import qualified AST.Expr as Expr
import AST.Expr (Expr)
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
  pp (DType x adt) = "type" <+> pretty x <+> "=" <+> pp adt
  pp (DSynonym x t) = "type" <+> pretty x <+> "=" <+> pp t

data Program = Program [Decl] (Maybe Expr)
  deriving (Show, Eq, Data, Typeable)

data ADTBody
  = Record [(Name, Type)]
  | Enum [(Name, Maybe Type)]
  deriving (Eq, Data, Typeable)

instance PP ADTBody where
  pp (Record l) = lbrace <+> hsep (ppField <$> l) <+> rbrace
    where
      ppField (x, t) = pretty x <> ":" <+> pp t
  pp (Enum l) = lbrace <+> hsep (ppVariant <$> l) <+> rbrace
    where
      ppVariant :: (Name, Maybe Type) -> Doc SimplePoly
      ppVariant (x, t) = pretty x <+> pp (("of" <+>) . pp <$> t)
