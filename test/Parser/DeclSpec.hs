module Parser.DeclSpec where

import AST.Decl
import Data.Either.Combinators
import Data.Function ((&))
import Data.Text (Text)
import Parser.Decl
import Test.Hspec
import Type.Types
import Util

parseDeclTest :: Text -> Decl
parseDeclTest s = parseDecl s & unwrap

spec :: Spec
spec = parallel $ do
  describe "declarations" $ do
    describe "adts" $ do
      describe "record" $ do
        it "should parse with or without semi" $ do
          let expected =
                DType
                  "Person"
                  (Record [("name", tStr), ("age", tInt)])
          parseDeclTest "type Person = { name: Str; age: Int }" `shouldBe` expected
          parseDeclTest "type Person = { name: Str; age: Int; }" `shouldBe` expected

        it "should not parse empty" $ do
          isLeft (parseDecl "type Person = {}") `shouldBe` True

      describe "enums" $ do
        it "it should parse with of type" $ do
          parseDeclTest "type Top = | First of Str | Second of Int | Third of Bool"
            `shouldBe` DType
              "Top"
              ( Enum
                  [ ("First", Just tStr),
                    ("Second", Just tInt),
                    ("Third", Just tBool)
                  ]
              )
        it "it should parse with empty variants" $ do
          parseDeclTest "type Top = | First | Second | Third"
            `shouldBe` DType
              "Top"
              ( Enum
                  [ ("First", Nothing),
                    ("Second", Nothing),
                    ("Third", Nothing)
                  ]
              )
  describe "type synonyms" $ do
    it "should parse synonym to prim" $ do
      parseDeclTest "type Name = Str" `shouldBe` DSynonym "Name" tStr

    it "should parse synonym to adt" $ do
      parseDeclTest "type Another = Person"
        `shouldBe` DSynonym
          "Another"
          (ADTTCon "Person")
