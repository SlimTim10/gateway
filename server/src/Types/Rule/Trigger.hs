module Types.Rule.Trigger where

import qualified Types.Prop as Prop

type Trigger = [TriggerElement]

data TriggerElement = TriggerElement
  { address :: Prop.Address
  , value :: Prop.Value
  }
  deriving (Eq)

instance Show TriggerElement where
  show TriggerElement {address, value}
    = "prop " ++ show address ++ " = " ++ show value
