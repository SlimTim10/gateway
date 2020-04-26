module RulesSpec (spec) where

import Test.Hspec
import Rules

import Data.Either (isRight)

import qualified Types.Prop as Prop
-- import Types.Rule
--   ( Rule(..)
--   )
import qualified Types.Rule as Rule
import Config
  ( ConfigRule(..)
  , ConfigTrigger(..)
  , ConfigAction(..)
  )

spec :: Spec
spec = do
  describe "FromConfig" $ do
    it "builds state out of a configuration" $ do
      let
        state = undefined
        crules =
          [ ConfigRule
            { ruleType = Rule.Basic
            , description = Just "First puzzle. Open the South door to get to the next room"
            , trigger =
              [ ConfigTrigger { name = "Card Spot 1", value = Prop.Int 3 }
              ]
            , action =
              [ ConfigAction { name = "South Door", value = Prop.String "open" }
              ]
            }
          , ConfigRule
            { ruleType = Rule.Sequence
            , description = Just "Get card 4 from the big lockbox"
            , trigger =
              [ ConfigTrigger { name = "Button Puzzle", value = Prop.Int 1 }
              , ConfigTrigger { name = "Button Puzzle", value = Prop.Int 2 }
              , ConfigTrigger { name = "Button Puzzle", value = Prop.Int 3 }
              , ConfigTrigger { name = "Button Puzzle", value = Prop.Int 4 }
              ]
            , action =
              [ ConfigAction { name = "Big Lockbox", value = Prop.String "unlocked" }
              ]
            }
          , ConfigRule
            { ruleType = Rule.Basic
            , description = Just "Get card 2 from the first small lockbox"
            , trigger =
              [ ConfigTrigger { name = "Card Spot 1", value = Prop.Int 4 }
              ]
            , action =
              [ ConfigAction { name = "Small Lockbox 1", value = Prop.String "unlocked" }
              ]
            }
          , ConfigRule
            { ruleType = Rule.Basic
            , description = Just "Get the piano chord from the second small lockbox"
            , trigger =
              [ ConfigTrigger { name = "Card Spot 1", value = Prop.Int 1 }
              , ConfigTrigger { name = "Card Spot 2", value = Prop.Int 2 }
              ]
            , action =
              [ ConfigAction { name = "Small Lockbox 2", value = Prop.String "unlocked" }
              ]
            }
          , ConfigRule
            { ruleType = Rule.Basic
            , description = Just "Play the right chord to get out!"
            , trigger =
              [ ConfigTrigger { name = "Mini Piano", value = Prop.IntList [1,0,0,0,1,0,0,1,0,0,0,0,0] }
              ]
            , action =
              [ ConfigAction { name = "East Door", value = Prop.String "open" }
              ]
            }
          ]
      let result = fromConfig state crules
      result `shouldSatisfy` isRight
      -- let (Right rules) = result
      -- rules `shouldBe`
