module State
  -- (
  -- ) where
  where

import Data.IntMap.Strict
  ( IntMap
  , (!?)
  )
import qualified Data.IntMap.Strict as IntMap

import Types.Prop
  ( Prop(..)
  )
import qualified Types.Prop as Prop
import Config
  ( ConfigProp(..)
  )
import Types.Rule
  ( Trigger(..)
  , Action(..)
  )

type State = IntMap Prop

fromConfig :: [ConfigProp] -> Either String State
fromConfig cProps = Right $ IntMap.fromAscList lst
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

checkTrigger :: Trigger -> State -> Bool
checkTrigger
  ( Trigger { propKey = key, value = tv } )
  state
  =
  case state !? key of
    Nothing -> False
    Just prop -> Prop.value prop == tv

applyAction :: Action -> State -> State
applyAction
  ( Action { propKey = key, value = av } )
  state
  =
  IntMap.update f key state
  where
    f prop = Just $ (prop :: Prop) { value = av }
