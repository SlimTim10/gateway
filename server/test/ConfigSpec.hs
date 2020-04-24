module ConfigSpec (spec) where

import Test.Hspec
import Config

import Data.Yaml (decodeEither', ParseException)
import Data.Either (isRight, isLeft)

import qualified Prop
import qualified Rule

spec :: Spec
spec = do
  describe "FromJSON ConfigProp" $ do
    it "parses prop information in YAML" $ do
      let yaml = "\
\ name: Tag Reader \n\
\ description: RFID tag reader \n\
\ address: 1 \n\
\ default-value: 1"
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right prop) = result
      prop `shouldBe`
        ConfigProp
        { name = "Tag Reader"
        , description = Just "RFID tag reader"
        , address = 1
        , defaultValue = Prop.Int 1
        }

    it "parses prop information in YAML" $ do
      let yaml = "\
\ name: Door \n\
\ description: Main door between rooms \n\
\ address: 2 \n\
\ default-value: closed"
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right prop) = result
      prop `shouldBe`
        ConfigProp
        { name = "Door"
        , description = Just "Main door between rooms"
        , address = 2
        , defaultValue = Prop.String "closed"
        }

    it "parses prop information in YAML" $ do
      let yaml = "\
\ name: Mini Piano \n\
\ description: A small piano \n\
\ address: 3 \n\
\ default-value: [0,0,0,0,0,0,0,0,0,0,0,0,0]"
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right prop) = result
      prop `shouldBe`
        ConfigProp
        { name = "Mini Piano"
        , description = Just "A small piano"
        , address = 3
        , defaultValue = Prop.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0]
        }

    it "handles optional fields" $ do
      let yaml = "\
\ name: Tag Reader \n\
\ address: 1 \n\
\ default-value: 1"
      let result = decodeEither' yaml :: Either ParseException ConfigProp
      result `shouldSatisfy` isRight
      let (Right prop) = result
      prop `shouldBe`
        ConfigProp
        { name = "Tag Reader"
        , description = Nothing
        , address = 1
        , defaultValue = Prop.Int 1
        }

    it "handles multiple props" $ do
      let yaml = "\
\ - name: Tag Reader \n\
\   description: RFID tag reader \n\
\   address: 1 \n\
\   default-value: 1 \n\
\ - name: Door \n\
\   description: Main door between rooms \n\
\   address: 2 \n\
\   default-value: closed"
      let result = decodeEither' yaml :: Either ParseException [ConfigProp]
      result `shouldSatisfy` isRight
      let (Right props) = result
      props `shouldBe`
        [ ConfigProp
          { name = "Tag Reader"
          , description = Just "RFID tag reader"
          , address = 1
          , defaultValue = Prop.Int 1
          }
        , ConfigProp
          { name = "Door"
          , description = Just "Main door between rooms"
          , address = 2
          , defaultValue = Prop.String "closed"
          }
        ]

    it "catches missing fields" $ do
      let yaml1 = "\
\ Tag Reader: \n\
\ default-value: 1"
      let result1 = decodeEither' yaml1 :: Either ParseException ConfigProp
      result1 `shouldSatisfy` isLeft

      let yaml2 = "\
\ Tag Reader: \n\
\ address: 1"
      let result2 = decodeEither' yaml2 :: Either ParseException ConfigProp
      result2 `shouldSatisfy` isLeft

    it "catches unhandled types" $ do
      let yaml1 = "\
\ name: Tag Reader \n\
\ address: 1 \n\
\ default-value: 1.23"
      let result1 = decodeEither' yaml1 :: Either ParseException ConfigProp
      result1 `shouldSatisfy` isLeft
      
      let yaml2 = "\
\ name: Tag Reader \n\
\ address: 1 \n\
\ default-value: [a, b, c]"
      let result2 = decodeEither' yaml2 :: Either ParseException ConfigProp
      result2 `shouldSatisfy` isLeft
      
      let yaml3 = "\
\ name: Tag Reader \n\
\ address: 1 \n\
\ default-value: [\"test\"]"
      let result3 = decodeEither' yaml3 :: Either ParseException ConfigProp
      result3 `shouldSatisfy` isLeft

  describe "FromJSON ConfigRule" $ do
    it "parses rule information in YAML" $ do
      let yaml = "\
\ type: basic \n\
\ description: Tag reader should open door \n\
\ trigger: \n\
\   - Tag Reader: 3 \n\
\ action: \n\
\   - South Door: open"
      let result = decodeEither' yaml :: Either ParseException ConfigRule
      result `shouldSatisfy` isRight
      let (Right rule) = result
      rule `shouldBe`
        ConfigRule
        { ruleType = Rule.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader" , value = Prop.Int 3 }
          ]
        , action =
          [ NameValue { name = "South Door" , value = Prop.String "open" }
          ]
        }

    it "parses rule information in YAML with a bigger trigger" $ do
      let yaml = "\
\ type: basic \n\
\ description: Tag reader should open door \n\
\ trigger: \n\
\   - Tag Reader 1: 3 \n\
\   - Tag Reader 2: 1 \n\
\ action: \n\
\   - South Door: open"
      let result = decodeEither' yaml :: Either ParseException ConfigRule
      result `shouldSatisfy` isRight
      let (Right rule) = result
      rule `shouldBe`
        ConfigRule
        { ruleType = Rule.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader 1", value = Prop.Int 3 }
          , NameValue { name = "Tag Reader 2", value = Prop.Int 1 }
          ]
        , action =
          [ NameValue { name = "South Door", value = Prop.String "open" }
          ]
        }

    it "parses rule information in YAML with a bigger action" $ do
      let yaml = "\
\ type: basic \n\
\ description: Tag reader should open door \n\
\ trigger: \n\
\   - Tag Reader: 3 \n\
\ action: \n\
\   - South Door: open \n\
\   - East Door: open"
      let result = decodeEither' yaml :: Either ParseException ConfigRule
      result `shouldSatisfy` isRight
      let (Right rule) = result
      rule `shouldBe`
        ConfigRule
        { ruleType = Rule.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader", value = Prop.Int 3 }
          ]
        , action =
          [ NameValue { name = "South Door", value = Prop.String "open" }
          , NameValue { name = "East Door", value = Prop.String "open" }
          ]
        }

    it "parses rule information in YAML with a bigger trigger and action" $ do
      let yaml = "\
\ type: basic \n\
\ description: Tag reader should open door \n\
\ trigger: \n\
\   - Tag Reader 1: 3 \n\
\   - Tag Reader 2: 1 \n\
\ action: \n\
\   - South Door: open \n\
\   - East Door: open"
      let result = decodeEither' yaml :: Either ParseException ConfigRule
      result `shouldSatisfy` isRight
      let (Right rule) = result
      rule `shouldBe`
        ConfigRule
        { ruleType = Rule.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader 1", value = Prop.Int 3 }
          , NameValue { name = "Tag Reader 2", value = Prop.Int 1 }
          ]
        , action =
          [ NameValue { name = "South Door", value = Prop.String "open" }
          , NameValue { name = "East Door", value = Prop.String "open" }
          ]
        }
