module AST
  ( Expr (..),
    Lit (..),
    BinOp (..),
    Decl (..),
    Program (..),
    Type (..),
    TCon (..),
    Scheme (..),
    TVar (..),
    ADTBody (..),
    Field (..),
    Variant (..),
    tVarSupply,
    tVar,
    tInt,
    tInt,
    tBool,
    tStr,
    tChar,
    intBinFun,
  )
where

import Control.Monad
import Data.Data (Data, Typeable)
import Data.Map (Map)
import Data.Name
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Generic.Random (genericArbitraryU)
import Poly.Pretty
import Prettyprinter
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Instances ()
import TextShow

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Bin BinOp Expr Expr
  deriving (Eq, Ord, Typeable, Data)
  deriving (Show) via PPShow Expr
  deriving (TextShow) via PPShow Expr

instance PP Expr where
  pp (Var x) = pretty x
  pp (App e1 e2) = annNest (pp e1) <+> pp e2
  pp (Lam x e) = annParensIf $ "\\" <> pretty x <+> "->" <+> pp e
  pp (Let a b c) = "let" <+> pretty a <+> "=" <+> pp b <+> "in" <+> pp c
  pp (Lit l) = pp l
  pp (Bin o a b) = annParensIf $ pp a <+> pp o <+> pp b
  pp (Fix a) = annParensIf $ "fix" <+> pp a
  pp (If a b c) = "if" <+> pp a <+> "then" <+> pp b <+> "else" <+> pp c

data Lit
  = LInt !Integer
  | LBool !Bool
  | LStr !Text
  | LChar !Char
  deriving (Eq, Ord, Typeable, Data)
  deriving (Show) via PPShow Lit
  deriving (TextShow) via PPShow Lit

instance PP Lit where
  pp (LInt i) = pretty i
  pp (LBool b) = pretty b
  pp (LStr s) = pretty s
  pp (LChar c) = pretty c

data BinOp = Add | Sub | Mul | Div | Eql | Neql
  deriving (Eq, Ord, Typeable, Data)
  deriving (Show) via PPShow BinOp
  deriving (TextShow) via PPShow BinOp

instance PP BinOp where
  pp Add = "+"
  pp Sub = "-"
  pp Mul = "*"
  pp Div = "/"
  pp Eql = "=="
  pp Neql = "!="

data Decl = DStmt Text Expr | DExpr Expr | DType Name ADTBody
  deriving (Eq)
  deriving (Show) via PPShow Decl
  deriving (TextShow) via PPShow Decl

instance PP Decl where
  pp (DStmt x e) = "let" <+> pretty x <+> "=" <+> pp e
  pp (DExpr e) = pp e <> ";"

data Program = Program [Decl] (Maybe Expr)
  deriving (Show, Eq)

data Type
  = TVar TVar
  | TCon TCon
  | Type :->: Type
  | TADT Name
  deriving (Eq, Ord, Generic, Typeable, Data)
  deriving (Show) via PPShow Type
  deriving (TextShow) via PPShow Type

infixr 9 :->:

instance Arbitrary Type where
  arbitrary = genericArbitraryU

instance PP Type where
  pp (t1 :->: t2) = shouldParens (isArr t1) (annNest $ pp t1) <+> "->" <+> pp t2
    where
      shouldParens True = annParens
      shouldParens False = id
      isArr (:->:) {} = True
      isArr _ = False
  pp (TVar v) = pretty v
  pp (TCon t) = pp t

data TCon
  = TInt
  | TBool
  | TStr
  | TChar
  deriving (Eq, Ord, Generic, Typeable, Data)
  deriving (Show) via PPShow TCon
  deriving (TextShow) via PPShow TCon

instance Arbitrary TCon where
  arbitrary = genericArbitraryU

instance PP TCon where
  pp TInt = "Int"
  pp TBool = "Bool"
  pp TStr = "Str"
  pp TChar = "Char"

tInt, tBool, tStr, tChar :: Type
tInt = TCon TInt
tBool = TCon TBool
tStr = TCon TStr
tChar = TCon TChar

intBinFun :: Type
intBinFun = TCon TInt :->: TCon TInt :->: TCon TInt

data Scheme = Forall (Set TVar) Type
  deriving (Eq, Typeable, Data)
  deriving (Show) via PPShow Scheme
  deriving (TextShow) via PPShow Scheme

instance PP Scheme where
  pp (Forall ts t)
    | Set.null ts = pp t
    | otherwise =
      "forall"
        <+> hcat
          ( punctuate space $
              pretty
                <$> Set.toList
                  ts
          )
        <> "."
        <+> pp t

newtype TVar = TV Text
  deriving
    ( Show,
      TextShow,
      Eq,
      Ord,
      Pretty,
      IsString,
      Typeable,
      Data,
      Arbitrary
    )

tVarSupply :: [TVar]
tVarSupply = TV . T.pack <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

instance PP TVar where
  pp (TV t) = pretty t

tVar :: Text -> Type
tVar = TVar . TV

data ADTBody
  = Record [Field]
  | Enum [Variant]
  deriving (Eq)

data Field = Field Name Type
  deriving (Eq)

data Variant = Variant Name Type
  deriving (Eq)
