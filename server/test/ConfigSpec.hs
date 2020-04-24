module ConfigSpec (spec) where

import Test.Hspec
import Config

import Data.Yaml (decodeEither', ParseException)
import Data.Either (isRight, isLeft)

import qualified Prop

spec :: Spec
spec = do
  describe "FromJSON ConfigProp" $ do
    it "parses prop information in YAML" $ do
      let yaml = "name: Tag Reader\ndescription: RFID tag reader\naddress: 1\ndefault-value: 1"
      let cprop = ConfigProp { name = "Tag Reader", description = Just "RFID tag reader", address = 1, defaultValue = Prop.Int 1 }
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprop

    it "parses prop information in YAML" $ do
      let yaml = "name: Door\ndescription: Main door between rooms\naddress: 2\ndefault-value: closed"
      let cprop = ConfigProp { name = "Door", description = Just "Main door between rooms", address = 2, defaultValue = Prop.String "closed" }
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprop

    it "parses prop information in YAML" $ do
      let yaml = "name: Mini Piano\ndescription: A small piano\naddress: 3\ndefault-value: [0,0,0,0,0,0,0,0,0,0,0,0,0]"
      let cprop = ConfigProp { name = "Mini Piano", description = Just "A small piano", address = 3, defaultValue = Prop.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0] }
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprop

    it "handles optional fields" $ do
      let yaml = "name: Tag Reader\naddress: 1\ndefault-value: 1"
      let cprop = ConfigProp { name = "Tag Reader", description = Nothing, address = 1, defaultValue = Prop.Int 1 }
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprop

    it "handles multiple props" $ do
      let yaml = "- name: Tag Reader\n  description: RFID tag reader\n  address: 1\n  default-value: 1\n- name: Door\n  description: Main door between rooms\n  address: 2\n  default-value: closed"
      let cprops = [ConfigProp { name = "Tag Reader", description = Just "RFID tag reader", address = 1, defaultValue = Prop.Int 1 }, ConfigProp { name = "Door", description = Just "Main door between rooms", address = 2, defaultValue = Prop.String "closed" }]
      let result = decodeEither' yaml :: Either ParseException [ConfigProp]
      result `shouldSatisfy` isRight
      let (Right r) = result
      r `shouldBe` cprops

    it "catches missing fields" $ do
      let yaml1 = "Tag Reader:\ndefault-value: 1"
      let result1 = decodeEither' yaml1 :: Either ParseException ConfigProp
      result1 `shouldSatisfy` isLeft

      let yaml2 = "Tag Reader:\naddress: 1\n"
      let result2 = decodeEither' yaml2 :: Either ParseException ConfigProp
      result2 `shouldSatisfy` isLeft

    it "catches unhandled types" $ do
      let yaml1 = "name: Tag Reader\naddress: 1\ndefault-value: 1.23"
      let result1 = decodeEither' yaml1 :: Either ParseException ConfigProp
      result1 `shouldSatisfy` isLeft
      
      let yaml2 = "name: Tag Reader\naddress: 1\ndefault-value: [a, b, c]"
      let result2 = decodeEither' yaml2 :: Either ParseException ConfigProp
      result2 `shouldSatisfy` isLeft
      
      let yaml3 = "name: Tag Reader\naddress: 1\ndefault-value: [\"test\"]"
      let result3 = decodeEither' yaml3 :: Either ParseException ConfigProp
      result3 `shouldSatisfy` isLeft
