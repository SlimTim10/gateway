module RulesSpec (spec) where

import Test.Hspec
import Rules

import qualified Data.IntMap.Strict as IntMap
import Data.Either (isRight, fromRight)

import Types.Prop
  ( Prop(..)
  )
import qualified Types.Prop as Prop
import Types.Rule (Rule(..))
import Types.Rule.Trigger (TriggerElement(..))
import Types.Rule.Action (ActionElement(..))
import qualified Types.Rule as Rule
import Config
  ( ConfigRule(..)
  , ConfigTriggerElement(..)
  , ConfigActionElement(..)
  )

spec :: Spec
spec = do
  describe "FromConfig" $ do
    it "builds rules out of a configuration" $ do
      let
        state =
          IntMap.fromAscList
          [ ( 1
            , Prop
              { name = "Card Spot 1"
              , description = Just "RFID tag reader"
              , address = 1
              , defaultValue = Prop.Int 0
              , value = Prop.Int 0
              }
            )
          , ( 2
            , Prop
              { name = "Card Spot 2"
              , description = Just "RFID tag reader"
              , address = 2
              , defaultValue = Prop.Int 0
              , value = Prop.Int 0
              }
            )
          , ( 3
            , Prop
              { name = "Card Spot 3"
              , description = Just "RFID tag reader"
              , address = 3
              , defaultValue = Prop.Int 0
              , value = Prop.Int 0
              }
            )
          , ( 4
            , Prop
              { name = "South Door"
              , description = Just "Enter the next room"
              , address = 0x10
              , defaultValue = Prop.String "closed"
              , value = Prop.String "closed"
              }
            )
          , ( 5
            , Prop
              { name = "Big Lockbox"
              , description = Just "Holds card 4"
              , address = 0x20
              , defaultValue = Prop.String "locked"
              , value = Prop.String "locked"
              }
            )
          , ( 6
            , Prop
              { name = "Small Lockbox 1"
              , description = Just "Holds card 2"
              , address = 0x30
              , defaultValue = Prop.String "locked"
              , value = Prop.String "locked"
              }
            )
          , ( 7
            , Prop
              { name = "Small Lockbox 2"
              , description = Just "Holds the hint to the piano chord"
              , address = 0x31
              , defaultValue = Prop.String "locked"
              , value = Prop.String "locked"
              }
            )
          , ( 8
            , Prop
              { name = "Button Puzzle"
              , description = Nothing
              , address = 0x40
              , defaultValue = Prop.Int 0
              , value = Prop.Int 0
              }
            )
          , ( 9
            , Prop
              { name = "Mini Piano"
              , description = Nothing
              , address = 0x50
              , defaultValue = Prop.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0]
              , value = Prop.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0]
              }
            )
          , ( 10
            , Prop
              { name = "East Door"
              , description = Just "Exit the escape room"
              , address = 0x60
              , defaultValue = Prop.String "closed"
              , value = Prop.String "closed"
              }
            )
          ]
        crules =
          [ ConfigRule
            { type_ = Rule.Basic
            , description = Just "First puzzle. Open the South door to get to the next room"
            , trigger =
              [ ConfigTriggerElement { name = "Card Spot 1", value = Prop.Int 3 }
              ]
            , action =
              [ ConfigActionElement { name = "South Door", value = Prop.String "open" }
              ]
            }
          , ConfigRule
            { type_ = Rule.Sequence
            , description = Just "Get card 4 from the big lockbox"
            , trigger =
              [ ConfigTriggerElement { name = "Button Puzzle", value = Prop.Int 1 }
              , ConfigTriggerElement { name = "Button Puzzle", value = Prop.Int 2 }
              , ConfigTriggerElement { name = "Button Puzzle", value = Prop.Int 3 }
              , ConfigTriggerElement { name = "Button Puzzle", value = Prop.Int 4 }
              ]
            , action =
              [ ConfigActionElement { name = "Big Lockbox", value = Prop.String "unlocked" }
              ]
            }
          , ConfigRule
            { type_ = Rule.Basic
            , description = Just "Get card 2 from the first small lockbox"
            , trigger =
              [ ConfigTriggerElement { name = "Card Spot 1", value = Prop.Int 4 }
              ]
            , action =
              [ ConfigActionElement { name = "Small Lockbox 1", value = Prop.String "unlocked" }
              ]
            }
          , ConfigRule
            { type_ = Rule.Basic
            , description = Just "Get the piano chord from the second small lockbox"
            , trigger =
              [ ConfigTriggerElement { name = "Card Spot 1", value = Prop.Int 1 }
              , ConfigTriggerElement { name = "Card Spot 2", value = Prop.Int 2 }
              ]
            , action =
              [ ConfigActionElement { name = "Small Lockbox 2", value = Prop.String "unlocked" }
              ]
            }
          , ConfigRule
            { type_ = Rule.Basic
            , description = Just "Play the right chord to get out!"
            , trigger =
              [ ConfigTriggerElement { name = "Mini Piano", value = Prop.IntList [1,0,0,0,1,0,0,1,0,0,0,0,0] }
              ]
            , action =
              [ ConfigActionElement { name = "East Door", value = Prop.String "open" }
              ]
            }
          ]
      let result = fromConfig state crules
      result `shouldSatisfy` isRight
      let rules = fromRight [] result
      rules `shouldBe`
        [ Rule
          { type_ = Rule.Basic
          , description = Just "First puzzle. Open the South door to get to the next room"
          , trigger =
            [ TriggerElement { propKey = 1, value = Prop.Int 3 }
            ]
          , action =
            [ ActionElement { propKey = 4, value = Prop.String "open" }
            ]
          }
        , Rule
          { type_ = Rule.Sequence
          , description = Just "Get card 4 from the big lockbox"
          , trigger =
            [ TriggerElement { propKey = 8, value = Prop.Int 1 }
            , TriggerElement { propKey = 8, value = Prop.Int 2 }
            , TriggerElement { propKey = 8, value = Prop.Int 3 }
            , TriggerElement { propKey = 8, value = Prop.Int 4 }
            ]
          , action =
            [ ActionElement { propKey = 5, value = Prop.String "unlocked" }
            ]
          }
        , Rule
          { type_ = Rule.Basic
          , description = Just "Get card 2 from the first small lockbox"
          , trigger =
            [ TriggerElement { propKey = 1, value = Prop.Int 4 }
            ]
          , action =
            [ ActionElement { propKey = 6, value = Prop.String "unlocked" }
            ]
          }
        , Rule
          { type_ = Rule.Basic
          , description = Just "Get the piano chord from the second small lockbox"
          , trigger =
            [ TriggerElement { propKey = 1, value = Prop.Int 1 }
            , TriggerElement { propKey = 2, value = Prop.Int 2 }
            ]
          , action =
            [ ActionElement { propKey = 7, value = Prop.String "unlocked" }
            ]
          }
        , Rule
          { type_ = Rule.Basic
          , description = Just "Play the right chord to get out!"
          , trigger =
            [ TriggerElement { propKey = 9, value = Prop.IntList [1,0,0,0,1,0,0,1,0,0,0,0,0] }
            ]
          , action =
            [ ActionElement { propKey = 10, value = Prop.String "open" }
            ]
          }
        ]
