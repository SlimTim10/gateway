module Types.Rule.Trigger where

import qualified Types.Prop as Prop

type Trigger = [TriggerElement]

data TriggerElement = TriggerElement
  { propKey :: Int
  , value :: Prop.Value
  }
  deriving (Show, Eq)
