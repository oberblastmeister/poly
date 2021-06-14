module ConstraintSpec (spec) where

import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Poly.Constraints
import Poly.Type
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $
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
        (tVar "x" ->> tVar "y")
        `shouldBe` (tInt ->> tBool)
