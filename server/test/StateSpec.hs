module StateSpec (spec) where

import Test.Hspec
import State

import qualified Data.IntMap.Strict as IntMap

import Types.Prop (Prop(..))
import qualified Types.Prop as Prop
import Config.Prop (ConfigProp(..))

spec :: Spec
spec = do
  describe "fromConfig" $ do
    it "builds state out of props configuration" $ do
      let
        cprops =
          [ ConfigProp
            { name = "Card Spot 1"
            , description = Just "RFID tag reader"
            , address = 1
            , defaultValue = Prop.Int 0
            }
          , ConfigProp
            { name = "Card Spot 2"
            , description = Just "RFID tag reader"
            , address = 2
            , defaultValue = Prop.Int 0
            }
          , ConfigProp
            { name = "Card Spot 3"
            , description = Just "RFID tag reader"
            , address = 3
            , defaultValue = Prop.Int 0
            }
          , ConfigProp
            { name = "South Door"
            , description = Just "Enter the next room"
            , address = 0x10
            , defaultValue = Prop.String "closed"
            }
          , ConfigProp
            { name = "Big Lockbox"
            , description = Just "Holds card 4"
            , address = 0x20
            , defaultValue = Prop.String "locked"
            }
          , ConfigProp
            { name = "Small Lockbox 1"
            , description = Just "Holds card 2"
            , address = 0x30
            , defaultValue = Prop.String "locked"
            }
          , ConfigProp
            { name = "Small Lockbox 2"
            , description = Just "Holds the hint to the piano chord"
            , address = 0x31
            , defaultValue = Prop.String "locked"
            }
          , ConfigProp
            { name = "Button Puzzle"
            , description = Nothing
            , address = 0x40
            , defaultValue = Prop.Int 0
            }
          , ConfigProp
            { name = "Mini Piano"
            , description = Nothing
            , address = 0x50
            , defaultValue = Prop.IntList [0,0,0,0,0,0,0,0,0,0,0,0,0]
            }
          , ConfigProp
            { name = "East Door"
            , description = Just "Exit the escape room"
            , address = 0x60
            , defaultValue = Prop.String "closed"
            }
          ]
      let state = fromConfig cprops
      state `shouldBe`
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
