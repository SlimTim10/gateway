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
  , Trigger(..)
  , Action(..)
  )
import qualified Types.Rule as Rule
import qualified Types.Prop as Prop
import Config
  ( ConfigRule(..)
  , ConfigTrigger(..)
  , ConfigAction(..)
  )

type Rules = [Rule]

fromConfig :: State -> [ConfigRule] -> Either String Rules
fromConfig state config = mapM (fromConfigRule state) config

fromConfigRule :: State -> ConfigRule -> Either String Rule
fromConfigRule state cRule = do
  let ct = (trigger :: ConfigRule -> [ConfigTrigger]) cRule
  let ca = (action :: ConfigRule -> [ConfigAction]) cRule
  trg <- mapM (fromConfigTrigger state) ct
  act <- mapM (fromConfigAction state) ca
  Right
    Rule
    { type_ = (type_ :: ConfigRule -> Rule.Type) cRule
    , description = (description :: ConfigRule -> Rule.Description) cRule
    , trigger = trg
    , action = act
    }

fromConfigTrigger :: State -> ConfigTrigger -> Either String Trigger
fromConfigTrigger state cTrigger = do
  let f = \(_, prop) -> Prop.name prop == (name :: ConfigTrigger -> Prop.Name) cTrigger
  case find f (IntMap.assocs state) of
    Nothing -> Left $ "Invalid trigger: " ++ show cTrigger
    Just (key, _) -> Right
      Trigger
      { propKey = key
      , value = (value :: ConfigTrigger -> Prop.Value) cTrigger
      }

fromConfigAction :: State -> ConfigAction -> Either String Action
fromConfigAction state cAction = do
  let f = \(_, prop) -> Prop.name prop == (name :: ConfigAction -> Prop.Name) cAction
  case find f (IntMap.assocs state) of
    Nothing -> Left $ "Invalid action: " ++ show cAction
    Just (key, _) -> Right
      Action
      { propKey = key
      , value = (value :: ConfigAction -> Prop.Value) cAction
      }
