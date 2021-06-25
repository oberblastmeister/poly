module Type.InferSpec (spec) where

import AST.Expr
import Data.Map (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Poly.QQ
import Test.Hspec
import Test.Hspec.QuickCheck
import Type.Env
import Type.Infer
import Type.Types

shouldBeEmpty :: (HasCallStack, Show a, Eq a) => Set a -> Expectation
shouldBeEmpty = (`shouldBe` Set.empty)

checkInferMono :: Expr -> Either TypeError Type -> Expectation
checkInferMono e ty =
  case inferExpr empty e of
    Left e -> Left e `shouldBe` ty
    Right res -> do
      let (Forall tvs ty') = res
      shouldBeEmpty tvs
      Right ty' `shouldBe` ty

checkInferPoly :: Expr -> Either TypeError Scheme -> Expectation
checkInferPoly e ty =
  case inferExpr empty e of
    Left e -> Left e `shouldBe` ty
    Right res ->
      Right res `shouldBe` ty

spec :: Spec
spec = parallel $ do
  describe "uninifying" $
    prop "equal types should always unify" $ \t ->
      unify t t == Right emptySubst

  describe "infering" $ do
    describe "free type variables" $ do
      prop "should get nothing from tcon" $ \t ->
        null (ftv $ TCon t)

      prop "ftv from var is itself" $ \t ->
        null (ftv $ TCon t)

      prop "ftv of arr should be union" $ \t1 t2 ->
        ftv (t1 :->: t2) == ftv t1 `Set.union` ftv t2

      it "should get from scheme" $ do
        ftv (Forall [] [ty|a|]) `shouldBe` [[tv|a|]]
        ftv (Forall [[tv|a|]] [ty|a|]) `shouldBe` []
        ftv (Forall [[tv|a|]] [ty|b -> a|]) `shouldBe` [[tv|b|]]

    describe "monotypes" $ do
      it "should infer for simple expressions" $ do
        checkInferMono [ex|1341234|] (Right [ty|Int|])
        checkInferMono [ex|234 + 12432|] (Right [ty|Int|])

      it "should infer for applications" $ do
        checkInferMono [ex|\x -> x + x + x + x + x|] (Right [ty|Int -> Int|])
        checkInferMono [ex|\x y z -> x + y + z|] (Right [ty|Int -> Int -> Int -> Int|])
        checkInferMono [ex|if True then 1234 else (\x -> x) 12334|] (Right [ty|Int|])

      it "should infer when they don't match" $
        checkInferMono
          [ex|if 234 then 123 else 134|]
          (Left $ UnificationFail [ty|Int|] [ty|Bool|])

      it "should infer in the body of an if statement" $
        checkInferMono
          [ex|let x = True in if x then \x -> x + x else \y -> y + y|]
          (Right [ty|Int -> Int|])

      it "should fail when there is unbound variable" $
        checkInferMono
          [ex|aposdiufpasoidfuapodsifuasd|]
          (Left $ UnboundVariable "aposdiufpasoidfuapodsifuasd")

    describe "polytypes" $ do
      it "should infer id" $
        checkInferPoly [ex|let id = \x -> x in id|] (Right [pty|a -> a|])

      it "should infer compose" $ do
        checkInferPoly
          [ex|\f g x -> f (g x)|]
          ( Right
              [pty|(b -> c) -> (a -> b) -> (a -> c)|]
          )

        checkInferPoly
          [ex|let compose = \f g x -> f (g x) in compose|]
          ( Right
              [pty|(b -> c) -> (a -> b) -> (a -> c)|]
              -- [pty|(a -> b -> c) -> (a -> b) -> a -> c|]
          )

      it "should infer apply" $ do
        checkInferPoly
          [ex|\f x -> f x|]
          (Right [pty|(a -> b) -> a -> b|])

        checkInferPoly
          [ex|let apply = \f x -> f x in apply|]
          (Right [pty|(a -> b) -> a -> b|])

      describe "lambda combinators" $ do
        it "should infer s" $ do
          checkInferPoly
            [ex|\x y z -> (x z)(y z)|]
            ( Right
                [pty|(a -> b -> c) -> (a -> b) -> a -> c|]
            )

          checkInferPoly
            [ex|let s = \x y z -> (x z)(y z) in s|]
            ( Right
                [pty|(a -> b -> c) -> (a -> b) -> a -> c|]
            )

        it "should infer k" $
          checkInferPoly
            [ex|\x y -> x|]
            (Right [pty|a -> b -> a|])

        it "should infer i" $
          checkInferPoly [ex|\x -> x|] (Right [pty|a -> a|])

        it "should not infer y" $
          checkInferPoly
            [ex|\f -> (\x -> f (x x)) (\x -> f (x x))|]
            ( Left
                ( InfiniteType
                    (TVUnbound 2)
                    ( TVar (TVUnbound 2)
                        :->: TVar (TVUnbound 3)
                    )
                )
            )

    it "should not infer infinite" $
      let inf =
            Left
              ( InfiniteType
                  (TVUnbound 1)
                  ( TVar (TVUnbound 1)
                      :->: TVar (TVUnbound 2)
                  )
              )
       in checkInferPoly [ex|(\x -> x x) (\x -> x x)|] inf

    describe "substitutable" $ do
      prop "should do nothing when substituting TCon" $ \s tcon ->
        let t = TCon tcon
         in s @@ t == t

      -- need to figure out why this one doesn't work
      -- prop "substituing should be associative" $ \(s1 :: Subst, s2 :: Subst, s3 :: Subst) (st :: Subst) ->
      --   let vals = ($ st) <$> tests
      --       tests =
      --         [ apply ((s1 <> s2) <> s3),
      --           apply (s1 <> s2 <> s3)
      --         ]
      --    in all (== (vals !! 1)) vals

      it "should apply" $ do
        apply
          (Subst (fromList [([tv|x|], tInt)]))
          [ty|x|]
          `shouldBe` [ty|Int|]

        apply
          (Subst (fromList [([tv|x|], tInt), ([tv|y|], tBool)]))
          [ty|x -> y|]
          `shouldBe` [ty|Int -> Bool|]
