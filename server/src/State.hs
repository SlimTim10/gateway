module State
  -- (
  -- ) where
  where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Types.Prop
  ( Prop(..)
  )
import qualified Types.Prop as Prop
import Config
  ( ConfigProp(..)
  )

type State = IntMap Prop

fromConfig :: [ConfigProp] -> Either String State
fromConfig cprops = Right $ IntMap.fromAscList lst
  where
    ps = map fromConfigProp cprops
    lst = zip [1..] ps

fromConfigProp :: ConfigProp -> Prop
fromConfigProp cprop = Prop
  { name = (name :: ConfigProp -> Prop.Name) cprop
  , description = (description :: ConfigProp -> Prop.Description) cprop
  , address = (address :: ConfigProp -> Prop.Address) cprop
  , defaultValue = (defaultValue :: ConfigProp -> Prop.Value) cprop
  , value = (defaultValue :: ConfigProp -> Prop.Value) cprop
  }
