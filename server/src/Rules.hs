module Rules
  -- (
  -- ) where
  where

import qualified Data.IntMap.Strict as IntMap
import Data.List (find)

import State (State)
import Types.Rule
  ( Rule(..)
  )
import Types.Rule.Trigger
  ( Trigger
  , TriggerElement(..)
  )
import Types.Rule.Action
  ( Action
  , ActionElement(..)
  )
import qualified Types.Prop as Prop
import Config.Rule (ConfigRule)
import qualified Config.Rule as CR
import Config.Rule.Trigger
  ( ConfigTrigger
  , ConfigTriggerElement
  )
import qualified Config.Rule.Trigger as CT
import Config.Rule.Action
  ( ConfigAction
  , ConfigActionElement
  )
import qualified Config.Rule.Action as CA

type Rules = [Rule]

fromConfig :: State -> [ConfigRule] -> Either String Rules
fromConfig state config = mapM (fromConfigRule state) config

fromConfigRule :: State -> ConfigRule -> Either String Rule
fromConfigRule state cRule = do
  trg <- fromConfigTrigger state (CR.trigger cRule)
  act <- fromConfigAction state (CR.action cRule)
  Right
    Rule
    { type_ = CR.type_ cRule
    , description = CR.description cRule
    , trigger = trg
    , action = act
    }

fromConfigTrigger :: State -> ConfigTrigger -> Either String Trigger
fromConfigTrigger state = mapM (fromConfigTriggerElement state)

fromConfigTriggerElement :: State -> ConfigTriggerElement -> Either String TriggerElement
fromConfigTriggerElement state cTrigger = do
  let f = \(_, prop) -> Prop.name prop == CT.name cTrigger
  case find f (IntMap.assocs state) of
    Nothing -> Left $ "Invalid trigger: " ++ show cTrigger
    Just (key, _) -> Right
      TriggerElement
      { propKey = key
      , value = CT.value cTrigger
      }

fromConfigAction :: State -> ConfigAction -> Either String Action
fromConfigAction state = mapM (fromConfigActionElement state)

fromConfigActionElement :: State -> ConfigActionElement -> Either String ActionElement
fromConfigActionElement state cAction = do
  let f = \(_, prop) -> Prop.name prop == CA.name cAction
  case find f (IntMap.assocs state) of
    Nothing -> Left $ "Invalid action: " ++ show cAction
    Just (key, _) -> Right
      ActionElement
      { propKey = key
      , value = CA.value cAction
      }
