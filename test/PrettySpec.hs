module PrettySpec (spec) where

import Data.Text (Text)
import Poly.Pretty
import Poly.Syntax
import Poly.Type
import Prettyprinter
import Prettyprinter.Render.Text
import Test.Hspec

-- ppT :: Pretty a => a -> Text
-- ppT = renderStrict . layoutPretty defaultLayoutOptions . pretty

spec :: Spec
spec = parallel $ do
  describe "pretty printer" $ do
    it "should pretty print TVar" $ do
      ppr (TV "asdf") `shouldBe` "asdf"

    it "should pretty print app" $ do
      ppr
        ( App
            (Lam "x" (Lam "y" (Var "x")))
            (Lit $ LBool True)
        )
        `shouldBe` "(\\x -> x) True"
