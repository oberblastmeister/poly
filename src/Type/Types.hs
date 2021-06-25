module Type.Types where

import Control.Monad
import Data.Data (Data, Typeable)
import Data.Name
import Data.Set (Set)
import qualified Data.Set as Set
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
  pp (TVar v) = pp v
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
              pp
                <$> Set.toList
                  ts
          )
        <> "."
        <+> pp t

data TVar
  -- need better arbitrary instance for this, tvnamed should not be empty
  = TVUnbound !Int
  | TVNamed !Text
  deriving (Show, Eq, Ord, Typeable, Data, Generic)

instance Arbitrary TVar where
  arbitrary = genericArbitraryU

-- newtype TVar = TV Text
--   deriving
--     ( Show,
--       TextShow,
--       Eq,
--       Ord,
--       Pretty,
--       IsString,
--       Typeable,
--       Data,
--       Arbitrary
--     )

instance PP TVar where
  pp (TVUnbound t) = pretty t
  pp (TVNamed t) = pretty t

unboundTVarSupply :: [TVar]
unboundTVarSupply = TVUnbound <$> [1 ..]

namedTVarSupply :: [TVar]
namedTVarSupply = TVNamed . T.pack <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

-- tVar :: Text -> Type
-- tVar = TVar . TV
