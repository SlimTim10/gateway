module Config.Rule.Trigger where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , withObject
  , parseJSON
  )
import qualified Data.HashMap.Strict as HM

import qualified Types.Prop as Prop

type ConfigTrigger = [ConfigTriggerElement]

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
