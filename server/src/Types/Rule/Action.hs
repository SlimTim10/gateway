module Types.Rule.Action where

import qualified Types.Prop as Prop

type Action = [ActionElement]

data ActionElement = ActionElement
  { address :: Prop.Address
  , value :: Prop.Value
  }
  deriving (Eq)

instance Show ActionElement where
  show ActionElement {address, value}
    = "prop " ++ show address ++ " = " ++ show value
