module Type.InferSpec where

import Data.Either.Combinators
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Poly.Type.Infer
import Poly.Syntax
import Poly.Type.Types
import Test.Hspec

shouldBeEmpty :: (HasCallStack, Show a, Eq a) => Set a -> Expectation
shouldBeEmpty = (`shouldBe` Set.empty)

testInferMono :: Expr -> Either TypeError Type -> Expectation
testInferMono e t = do
  case inferExpr emptyTypeEnv e of
    Left e -> Left e `shouldBe` t
    Right res -> do
      let (Forall tvs t') = res
      shouldBeEmpty tvs
      Right t' `shouldBe` t

spec :: Spec
spec = parallel $ do
  it "should infer literals" $ do
    Lit (LInt 1234) `testInferMono` Right (TCon TInt)
    Lit (LStr "asdf") `testInferMono` Right (TCon TStr)
    Lit (LChar 'a') `testInferMono` Right (TCon TChar)
