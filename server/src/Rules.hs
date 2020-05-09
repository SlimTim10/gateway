module Rules
  ( Rules
  , fromConfig
  , fromConfigThrow
  , prettyPrint
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Control.Exception (throwIO)
import Data.Text (unpack)

import State (State)
import Types.Rule (Rule(..))
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
import Config (ConfigException(..))

type Rules = [Rule]

fromConfigThrow :: State -> [ConfigRule] -> IO Rules
fromConfigThrow state cRules = either throwIO return $ fromConfig state cRules

fromConfig :: State -> [ConfigRule] -> Either ConfigException Rules
fromConfig state cRules = mapM (fromConfigRule state) cRules

fromConfigRule :: State -> ConfigRule -> Either ConfigException Rule
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

fromConfigTrigger :: State -> ConfigTrigger -> Either ConfigException Trigger
fromConfigTrigger state = mapM (fromConfigTriggerElement state)

fromConfigTriggerElement :: State -> ConfigTriggerElement -> Either ConfigException TriggerElement
fromConfigTriggerElement state cTrigger = do
  let f = \(_, prop) -> Prop.name prop == CT.name cTrigger
  case find f (IntMap.assocs state) of
    Nothing -> Left $ InvalidTrigger $ show cTrigger
    Just (addr, _) -> Right
      TriggerElement
      { address = addr
      , name = CT.name cTrigger
      , value = CT.value cTrigger
      }

fromConfigAction :: State -> ConfigAction -> Either ConfigException Action
fromConfigAction state = mapM (fromConfigActionElement state)

fromConfigActionElement :: State -> ConfigActionElement -> Either ConfigException ActionElement
fromConfigActionElement state cAction = do
  let f = \(_, prop) -> Prop.name prop == CA.name cAction
  case find f (IntMap.assocs state) of
    Nothing -> Left $ InvalidAction $ show cAction
    Just (addr, _) -> Right
      ActionElement
      { address = addr
      , name = CA.name cAction
      , value = CA.value cAction
      }

prettyPrint :: Rules -> IO ()
prettyPrint rules = mapM_ f rules >> putStrLn ""
  where
    f Rule{type_, description, trigger, action} = do
      let
        indent = replicate 2 ' '
        descString = case description of
          Just d -> " (" ++ unpack d ++ ")"
          Nothing -> ""
      putStrLn $ show type_ ++ descString
      putStrLn $ indent ++ "trigger: " ++ show trigger
      putStrLn $ indent ++ "action: " ++ show action
      putStrLn ""
