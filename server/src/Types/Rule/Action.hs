module Types.Rule.Action where

import Data.Text (unpack)

import qualified Types.Prop as Prop

type Action = [ActionElement]

data ActionElement = ActionElement
  { address :: Prop.Address
  , name :: Prop.Name
  , value :: Prop.Value
  }
  deriving (Eq)

instance Show ActionElement where
  show ActionElement {name, value}
    = unpack name ++ " = " ++ show value
