module Types.Rule.Trigger where

import Data.Text (unpack)

import qualified Types.Prop as Prop

type Trigger = [TriggerElement]

data TriggerElement = TriggerElement
  { address :: Prop.Address
  , name :: Prop.Name
  , value :: Prop.Value
  }
  deriving (Eq)

instance Show TriggerElement where
  show TriggerElement {name, value}
    = unpack name ++ " = " ++ show value
