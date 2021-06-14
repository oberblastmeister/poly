module Poly.Type where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics
import Generic.Random
import Poly.Pretty
import Prettyprinter
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import TextShow

data Scheme = Forall (Set TVar) Type deriving (Show)

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
        <+> "."
        <+> pp t

newtype TVar = TV Text
  deriving (Show, TextShow, Eq, Ord, Pretty, Arbitrary)

instance PP TVar where
  pp (TV t) = pretty t

data Type
  = TVar TVar
  | TCon TCon
  | TArr Type Type
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Type where
  arbitrary = genericArbitraryU

tVar :: Text -> Type
tVar = TVar . TV

infixr 9 `TArr`

(->>) :: Type -> Type -> Type
(->>) = TArr

infixr 9 ->>

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
  | TStr
  | TChar
  deriving (Show, Eq, Ord, Generic)

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
intBinFun = TCon TInt `TArr` (TCon TInt `TArr` TCon TInt)
