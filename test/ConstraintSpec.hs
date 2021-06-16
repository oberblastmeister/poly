module ConstraintSpec (spec) where

import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Poly.Constraints
import Poly.Type
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "uninifying" $ do
    prop "equal types should always unify" $
      equalTypesUnifyProp

  describe "free type variables" $ do
    prop "should get nothing from tcon" $
      ftvTConProp

    prop "ftv from var is itself" $
      tVarFTVProp

    prop "ftv of arr should be union" $
      tArrFTVProp

    it "should get from scheme" $ do
      ftv (Forall [] (TVar "a")) `shouldBe` ["a"]
      ftv (Forall ["a"] (TVar "a")) `shouldBe` []
      ftv (Forall ["a"] (TVar "b" :->: TVar "a")) `shouldBe` ["b"]

  describe "substitutable" $ do
    prop "should do nothing when substituting TCon" $
      substTConProp

    prop "substituing should be associative" $
      substAssociative

    it "should apply" $ do
      apply
        (Subst (fromList [(TV "x", tInt)]))
        ( TVar $
            TV "x"
        )
        `shouldBe` TCon TInt

      apply
        (Subst (fromList [(TV "x", tInt), (TV "y", tBool)]))
        (tVar "x" :->: tVar "y")
        `shouldBe` (tInt :->: tBool)
