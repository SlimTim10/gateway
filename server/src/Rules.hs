module Rules
  -- (
  -- ) where
  where

import qualified Data.IntMap.Strict as IntMap
import Data.List (find)

import State
  ( State
  )
import Types.Rule
  ( Rule(..)
  , TriggerElement(..)
  , Trigger
  , ActionElement(..)
  , Action
  )
import qualified Types.Rule as Rule
import qualified Types.Prop as Prop
import Config
  ( ConfigRule(..)
  , ConfigTriggerElement(..)
  , ConfigTrigger
  , ConfigActionElement(..)
  , ConfigAction
  )

type Rules = [Rule]

fromConfig :: State -> [ConfigRule] -> Either String Rules
fromConfig state config = mapM (fromConfigRule state) config

fromConfigRule :: State -> ConfigRule -> Either String Rule
fromConfigRule state cRule = do
  trg <- fromConfigTrigger state (trigger cRule)
  act <- fromConfigAction state (action cRule)
  Right
    Rule
    { type_ = type_ cRule
    , description = description cRule
    , trigger = trg
    , action = act
    }
  where
    trigger = trigger :: ConfigRule -> ConfigTrigger
    action = action :: ConfigRule -> ConfigAction
    type_ = type_ :: ConfigRule -> Rule.Type
    description = description :: ConfigRule -> Rule.Description

fromConfigTrigger :: State -> ConfigTrigger -> Either String Trigger
fromConfigTrigger state = mapM (fromConfigTriggerElement state)

fromConfigTriggerElement :: State -> ConfigTriggerElement -> Either String TriggerElement
fromConfigTriggerElement state cTrigger = do
  let f = \(_, prop) -> Prop.name prop == name cTrigger
  case find f (IntMap.assocs state) of
    Nothing -> Left $ "Invalid trigger: " ++ show cTrigger
    Just (key, _) -> Right
      TriggerElement
      { propKey = key
      , value = value cTrigger
      }
  where
    name = name :: ConfigTriggerElement -> Prop.Name
    value = value :: ConfigTriggerElement -> Prop.Value

fromConfigAction :: State -> ConfigAction -> Either String Action
fromConfigAction state = mapM (fromConfigActionElement state)

fromConfigActionElement :: State -> ConfigActionElement -> Either String ActionElement
fromConfigActionElement state cAction = do
  let f = \(_, prop) -> Prop.name prop == name cAction
  case find f (IntMap.assocs state) of
    Nothing -> Left $ "Invalid action: " ++ show cAction
    Just (key, _) -> Right
      ActionElement
      { propKey = key
      , value = value cAction
      }
  where
    name = name :: ConfigActionElement -> Prop.Name
    value = value :: ConfigActionElement -> Prop.Value
