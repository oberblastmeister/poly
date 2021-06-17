module ConstraintSpec (spec) where

import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Poly.Constraints
import Poly.QQ
import Poly.Syntax
import Poly.Type
import Poly.TypeEnv
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

shouldBeEmpty :: (HasCallStack, Show a, Eq a) => Set a -> Expectation
shouldBeEmpty = (`shouldBe` Set.empty)

checkInferMono :: Expr -> Either TypeError Type -> Expectation
checkInferMono e t = do
  case inferExpr empty e of
    Left e -> Left e `shouldBe` t
    Right res -> do
      let (Forall tvs t') = res
      shouldBeEmpty tvs
      Right t' `shouldBe` t

spec :: Spec
spec = parallel $ do
  describe "uninifying" $ do
    prop "equal types should always unify" $
      equalTypesUnifyProp

  describe "infering" $ do
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

    describe "monotypes" $ do
      it "should infer for simple expressions" $ do
        checkInferMono [ex|1341234|] (Right [ty|Int|])
        checkInferMono [ex|234 + 12432|] (Right [ty|Int|])

      it "should infer for applications" $ do
        checkInferMono [ex|\x -> x + x + x + x + x|] (Right [ty|Int -> Int|])
        checkInferMono [ex|\x y z -> x + y + z|] (Right [ty|Int -> Int -> Int -> Int|])
        checkInferMono [ex|if True then 1234 else (\x -> x) 12334|] (Right [ty|Int|])

      it "should infer when they don't match" $ do
        checkInferMono [ex|if 234 then 123 else 134|] (Left $ UnificationFail [ty|Int|] [ty|Bool|])

      it "should infer in the body of an if statement" $ do
        checkInferMono [ex|let x = True in if x then \x -> x + x else \y -> y + y|] (Right [ty|Int -> Int|])

      it "should fail when there is unbound variable" $ do
        checkInferMono [ex|aposdiufpasoidfuapodsifuasd|] (Left $ UnboundVariable "aposdiufpasoidfuapodsifuasd")

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
