module ConfigSpec (spec) where

import Test.Hspec
import Config

import Data.Yaml (decodeEither', ParseException)
import Data.Either (isRight, isLeft)

import qualified Config.Prop as P
import qualified Config.Rule as R

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
        , defaultValue = P.Int 1
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
        , defaultValue = P.String "closed"
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
        , defaultValue = P.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0]
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
        , defaultValue = P.Int 1
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
          , defaultValue = P.Int 1
          }
        , ConfigProp
          { name = "Door"
          , description = Just "Main door between rooms"
          , address = 2
          , defaultValue = P.String "closed"
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
        { ruleType = R.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader" , value = P.Int 3 }
          ]
        , action =
          [ NameValue { name = "South Door" , value = P.String "open" }
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
        { ruleType = R.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader 1", value = P.Int 3 }
          , NameValue { name = "Tag Reader 2", value = P.Int 1 }
          ]
        , action =
          [ NameValue { name = "South Door", value = P.String "open" }
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
        { ruleType = R.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader", value = P.Int 3 }
          ]
        , action =
          [ NameValue { name = "South Door", value = P.String "open" }
          , NameValue { name = "East Door", value = P.String "open" }
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
        { ruleType = R.Basic
        , description = Just "Tag reader should open door"
        , trigger =
          [ NameValue { name = "Tag Reader 1", value = P.Int 3 }
          , NameValue { name = "Tag Reader 2", value = P.Int 1 }
          ]
        , action =
          [ NameValue { name = "South Door", value = P.String "open" }
          , NameValue { name = "East Door", value = P.String "open" }
          ]
        }

  describe "readConfig" $ do
    it "parses an entire config file in YAML" $ do
      True `shouldBe` True
      result <- readConfig "test/data/config.yaml"
      result `shouldSatisfy` isRight
      let (Right config) = result
      config `shouldBe`
        Config
        { props =
          [ ConfigProp
            { name = "Card Spot 1"
            , description = Just "RFID tag reader"
            , address = 1
            , defaultValue = P.Int 0
            }
          , ConfigProp
            { name = "Card Spot 2"
            , description = Just "RFID tag reader"
            , address = 2
            , defaultValue = P.Int 0
            }
          , ConfigProp
            { name = "Card Spot 3"
            , description = Just "RFID tag reader"
            , address = 3
            , defaultValue = P.Int 0
            }
          , ConfigProp
            { name = "South Door"
            , description = Just "Enter the next room"
            , address = 0x10
            , defaultValue = P.String "closed"
            }
          , ConfigProp
            { name = "Big Lockbox"
            , description = Just "Holds card 4"
            , address = 0x20
            , defaultValue = P.String "locked"
            }
          , ConfigProp
            { name = "Small Lockbox 1"
            , description = Just "Holds card 2"
            , address = 0x30
            , defaultValue = P.String "locked"
            }
          , ConfigProp
            { name = "Small Lockbox 2"
            , description = Just "Holds the hint to the piano chord"
            , address = 0x31
            , defaultValue = P.String "locked"
            }
          , ConfigProp
            { name = "Button Puzzle"
            , description = Nothing
            , address = 0x40
            , defaultValue = P.Int 0
            }
          , ConfigProp
            { name = "Mini Piano"
            , description = Nothing
            , address = 0x50
            , defaultValue = P.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0]
            }
          , ConfigProp
            { name = "East Door"
            , description = Just "Exit the escape room"
            , address = 0x60
            , defaultValue = P.String "closed"
            }
          ]
        , rules =
          [ ConfigRule
            { ruleType = R.Basic
            , description = Just "First puzzle. Open the South door to get to the next room"
            , trigger =
              [ NameValue { name = "Card Spot 1", value = P.Int 3 }
              ]
            , action =
              [ NameValue { name = "South Door", value = P.String "open" }
              ]
            }
          , ConfigRule
            { ruleType = R.Sequence
            , description = Just "Get card 4 from the big lockbox"
            , trigger =
              [ NameValue { name = "Button Puzzle", value = P.Int 1 }
              , NameValue { name = "Button Puzzle", value = P.Int 2 }
              , NameValue { name = "Button Puzzle", value = P.Int 3 }
              , NameValue { name = "Button Puzzle", value = P.Int 4 }
              ]
            , action =
              [ NameValue { name = "Big Lockbox", value = P.String "unlocked" }
              ]
            }
          , ConfigRule
            { ruleType = R.Basic
            , description = Just "Get card 2 from the first small lockbox"
            , trigger =
              [ NameValue { name = "Card Spot 1", value = P.Int 4 }
              ]
            , action =
              [ NameValue { name = "Small Lockbox 1", value = P.String "unlocked" }
              ]
            }
          , ConfigRule
            { ruleType = R.Basic
            , description = Just "Get the piano chord from the second small lockbox"
            , trigger =
              [ NameValue { name = "Card Spot 1", value = P.Int 1 }
              , NameValue { name = "Card Spot 2", value = P.Int 2 }
              ]
            , action =
              [ NameValue { name = "Small Lockbox 2", value = P.String "unlocked" }
              ]
            }
          , ConfigRule
            { ruleType = R.Basic
            , description = Just "Play the right chord to get out!"
            , trigger =
              [ NameValue { name = "Mini Piano", value = P.IntList [1,0,0,0,1,0,0,1,0,0,0,0,0] }
              ]
            , action =
              [ NameValue { name = "East Door", value = P.String "open" }
              ]
            }
          ]
        }
