module State.Prop where

import Types.Prop
  ( Name
  , Description
  , Address
  , Value
  )

data Prop = Prop
  { name :: Name
  , description :: Description
  , address :: Address
  , defaultValue :: Value
  , value :: Value
  }
