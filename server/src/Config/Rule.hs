module Config.Rule where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , (.:)
  , (.:?)
  , withObject
  , parseJSON
  )
import qualified Data.HashMap.Strict as HM

import Types.Rule
  ( Type
  , Description
  )
import qualified Types.Prop as Prop

data ConfigTriggerElement
  = ConfigTriggerElement
  { name :: Prop.Name
  , value :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ConfigTriggerElement where
  parseJSON = withObject "name-value" $ \o -> do
    case HM.toList o of
      [(name, value')] -> do
        value <- parseJSON value'
        return $ ConfigTriggerElement name value
      _ -> error "Could not parse ConfigTriggerElement"

data ConfigActionElement
  = ConfigActionElement
  { name :: Prop.Name
  , value :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ConfigActionElement where
  parseJSON = withObject "name-value" $ \o -> do
    case HM.toList o of
      [(name, value')] -> do
        value <- parseJSON value'
        return $ ConfigActionElement name value
      _ -> error "Could not parse ConfigActionElement"

type ConfigTrigger = [ConfigTriggerElement]
type ConfigAction = [ConfigActionElement]

data ConfigRule
  = ConfigRule
  { type_ :: Type
  , description :: Description
  , trigger :: ConfigTrigger
  , action :: ConfigAction
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ConfigRule where
  parseJSON = withObject "rule" $ \o -> do
    type_ <- o .: "type"
    description <- o .:? "description"
    trigger <- o .: "trigger"
    action <- o .: "action"
    return $ ConfigRule type_ description trigger action

