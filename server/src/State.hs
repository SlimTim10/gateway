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
import Config.Prop (ConfigProp)
import qualified Config.Prop as CP

type State = IntMap Prop

fromConfig :: [ConfigProp] -> State
fromConfig cProps = IntMap.fromAscList lst
  where
    ps = map fromConfigProp cProps
    lst = zip [1..] ps

fromConfigProp :: ConfigProp -> Prop
fromConfigProp cProp = Prop
  { name = CP.name cProp
  , description = CP.description cProp
  , address = CP.address cProp
  , defaultValue = CP.defaultValue cProp
  , value = CP.defaultValue cProp
  }
