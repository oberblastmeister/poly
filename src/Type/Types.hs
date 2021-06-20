module Type.Types where

import Control.Monad
import Data.Data (Data, Typeable)
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

data Type
  = TVar TVar
  | TCon TCon
  | ADTTCon Name
  | Type :->: Type
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
  pp (ADTTCon x) = pretty x
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
