module Type.InferSpec (spec) where

import AST.Expr
import Type.Types
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Poly.QQ
import Test.Hspec
import Test.Hspec.QuickCheck
import Type.Infer
import Type.TypeEnv

shouldBeEmpty :: (HasCallStack, Show a, Eq a) => Set a -> Expectation
shouldBeEmpty = (`shouldBe` Set.empty)

checkInferMono :: Expr -> Either TypeError Type -> Expectation
checkInferMono e ty = do
  case inferExpr empty e of
    Left e -> Left e `shouldBe` ty
    Right res -> do
      let (Forall tvs ty') = res
      shouldBeEmpty tvs
      Right ty' `shouldBe` ty

checkInferPoly :: Expr -> Either TypeError Scheme -> Expectation
checkInferPoly e ty = do
  case inferExpr empty e of
    Left e -> Left e `shouldBe` ty
    Right res -> do
      Right res `shouldBe` ty

spec :: Spec
spec = parallel $ do
  describe "uninifying" $ do
    prop "equal types should always unify" $ \t ->
      unify t t == Right emptySubst

  describe "infering" $ do
    describe "free type variables" $ do
      prop "should get nothing from tcon" $ \t ->
        null (ftv $ TCon t)

      -- prop "ftv from var is itself" $ \t ->
      --   null (ftv $ TCon t)

      prop "ftv of arr should be union" $ \t1 t2 ->
        ftv (t1 :->: t2) == ftv t1 `Set.union` ftv t2

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
        checkInferMono
          [ex|if 234 then 123 else 134|]
          (Left $ UnificationFail [ty|Int|] [ty|Bool|])

      it "should infer in the body of an if statement" $ do
        checkInferMono
          [ex|let x = True in if x then \x -> x + x else \y -> y + y|]
          (Right [ty|Int -> Int|])

      it "should fail when there is unbound variable" $ do
        checkInferMono
          [ex|aposdiufpasoidfuapodsifuasd|]
          (Left $ UnboundVariable "aposdiufpasoidfuapodsifuasd")

    describe "polytypes" $ do
      it "should infer id" $ do
        checkInferPoly [ex|let id = \x -> x in id|] (Right $ Forall ["a"] [ty|a -> a|])

      it "should infer compose" $ do
        checkInferPoly
          [ex|\f g x -> f (g x)|]
          ( Right $
              Forall
                ["a", "b", "c"]
                [ty|(b -> c) -> (a -> b) -> (a -> c)|]
          )

        checkInferPoly
          [ex|let compose = \f g x -> f (g x) in compose|]
          ( Right $
              Forall
                ["a", "b", "c"]
                [ty|(b -> c) -> (a -> b) -> (a -> c)|]
                -- [ty|(a -> b -> c) -> (a -> b) -> a -> c|]
          )

      it "should infer apply" $ do
        checkInferPoly
          [ex|\f x -> f x|]
          (Right $ Forall ["a", "b"] [ty|(a -> b) -> a -> b|])

        checkInferPoly
          [ex|let apply = \f x -> f x in apply|]
          (Right $ Forall ["a", "b"] [ty|(a -> b) -> a -> b|])

      describe "lambda combinators" $ do
        it "should infer s" $ do
          checkInferPoly
            [ex|\x y z -> (x z)(y z)|]
            ( Right $
                Forall
                  ["a", "b", "c"]
                  [ty|(a -> b -> c) -> (a -> b) -> a -> c|]
            )

          checkInferPoly
            [ex|let s = \x y z -> (x z)(y z) in s|]
            ( Right $
                Forall
                  ["a", "b", "c"]
                  [ty|(a -> b -> c) -> (a -> b) -> a -> c|]
            )

        it "should infer k" $ do
          checkInferPoly
            [ex|\x y -> x|]
            (Right $ Forall ["a", "b"] [ty|a -> b -> a|])

        it "should infer i" $ do
          checkInferPoly [ex|\x -> x|] (Right $ Forall ["a"] [ty|a -> a|])

        it "should not infer y" $ do
          checkInferPoly
            [ex|\f -> (\x -> f (x x)) (\x -> f (x x))|]
            (Left (InfiniteType (TV "b") (TVar (TV "b") :->: TVar (TV "c"))))

    -- it "should not infer infinite" $ do
    --   checkInferPoly [ex|(\x -> x x) (\x -> x x)|] (Right $ Forall ["a"] [ty|a -> a|])

    describe "substitutable" $ do
      prop "should do nothing when substituting TCon" $ \s tcon ->
        let t = TCon tcon
         in s @@ t == t

      prop "substituing should be associative" $ \(s1 :: Subst, s2 :: Subst, s3 :: Subst) (st :: Subst) ->
        let vals = ($ st) <$> tests
            tests =
              [ apply ((s1 <> s2) <> s3),
                apply (s1 <> s2 <> s3)
              ]
         in all (== (vals !! 1)) vals

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
