module Types.Rule.Action where

import qualified Types.Prop as Prop

type Action = [ActionElement]

data ActionElement = ActionElement
  { propKey :: Int
  , value :: Prop.Value
  }
  deriving (Show, Eq)
