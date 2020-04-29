module State
  ( State
  , (!?)
  , fromConfig
  , update
  ) where

import Data.IntMap.Strict
  ( IntMap
  , (!?)
  , update
  )
import qualified Data.IntMap.Strict as IntMap

import Types.Prop (Prop(..))
import qualified Types.Prop as Prop
import Config (ConfigProp(..))

type State = IntMap Prop

fromConfig :: [ConfigProp] -> State
fromConfig cProps = IntMap.fromAscList lst
  where
    ps = map fromConfigProp cProps
    lst = zip [1..] ps

fromConfigProp :: ConfigProp -> Prop
fromConfigProp cProp = Prop
  { name = (name :: ConfigProp -> Prop.Name) cProp
  , description = (description :: ConfigProp -> Prop.Description) cProp
  , address = (address :: ConfigProp -> Prop.Address) cProp
  , defaultValue = (defaultValue :: ConfigProp -> Prop.Value) cProp
  , value = (defaultValue :: ConfigProp -> Prop.Value) cProp
  }
