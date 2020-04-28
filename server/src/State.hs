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
  ( TriggerElement(..)
  , Trigger
  , ActionElement(..)
  , Action
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

checkTrigger :: State -> Trigger -> Bool
checkTrigger = undefined

checkTriggerElement :: State -> TriggerElement -> Bool
checkTriggerElement
  state
  ( TriggerElement { propKey = key, value = tv } )
  =
  case state !? key of
    Nothing -> False
    Just prop -> Prop.value prop == tv

applyAction :: State -> Action -> State
applyAction = undefined

applyActionElement :: State -> ActionElement -> State
applyActionElement
  state
  ( ActionElement { propKey = key, value = av } )
  =
  IntMap.update f key state
  where
    f prop = Just $ (prop :: Prop) { value = av }
