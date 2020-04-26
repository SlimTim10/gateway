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
  ( Config(..)
  , ConfigProp(..)
  )

type State = IntMap Prop

fromConfig :: Config -> Either String State
fromConfig config = Right $ IntMap.fromAscList lst
  where
    ps = map fromConfigProp (props config)
    lst = zip [1..] ps

fromConfigProp :: ConfigProp -> Prop
fromConfigProp cprop = Prop
  { name = (name :: ConfigProp -> Prop.Name) cprop
  , description = (description :: ConfigProp -> Prop.Description) cprop
  , address = (address :: ConfigProp -> Prop.Address) cprop
  , defaultValue = (defaultValue :: ConfigProp -> Prop.Value) cprop
  , value = (defaultValue :: ConfigProp -> Prop.Value) cprop
  }

validate :: Config -> Either String Config
validate = undefined

uniquePropNames :: Config -> Either String Config
uniquePropNames = undefined

uniquePropAddresses :: Config -> Either String Config
uniquePropAddresses = undefined

noRedundantTriggers :: Config -> Either String Config
noRedundantTriggers = undefined

validPropNamesInTriggers :: Config -> Either String Config
validPropNamesInTriggers = undefined
