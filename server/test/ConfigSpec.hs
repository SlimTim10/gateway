module ConfigSpec (spec) where

import Test.Hspec
import Config

import Data.Yaml (decodeEither', ParseException)
import Data.Either (isRight, isLeft)

import qualified Prop

spec :: Spec
spec = do
  describe "FromJSON ConfigProp" $ do
    it "parses a prop in YAML" $ do
      let yaml = "Tag Reader:\n  address: 1\n  value-type: integer\n  default-value: 1"
      let cprop = ConfigProp { name = "Tag Reader", address = 1, defaultValue = Prop.Int 1 }
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprop

    it "parses a prop in YAML" $ do
      let yaml = "Door:\n  address: 2\n  value-type: string\n  default-value: closed"
      let cprop = ConfigProp { name = "Door", address = 2, defaultValue = Prop.String "closed" }
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprop

    it "parses a prop in YAML" $ do
      let yaml = "Mini Piano:\n  address: 3\n  value-type: integer-list\n  default-value: [0,0,0,0,0,0,0,0,0,0,0,0,0]"
      let cprop = ConfigProp { name = "Mini Piano", address = 3, defaultValue = Prop.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0] }
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprop

    it "catches missing fields" $ do
      let yaml1 = "Tag Reader:\n  value-type: integer\n  default-value: 1"
      let result1 = decodeEither' yaml1 :: Either ParseException ConfigProp
      result1 `shouldSatisfy` isLeft

      let yaml2 = "Tag Reader:\n  address: 1\n  default-value: 1"
      let result2 = decodeEither' yaml2 :: Either ParseException ConfigProp
      result2 `shouldSatisfy` isLeft

      let yaml3 = "Tag Reader:\n  address: 1\n  value-type: integer"
      let result3 = decodeEither' yaml3 :: Either ParseException ConfigProp
      result3 `shouldSatisfy` isLeft
