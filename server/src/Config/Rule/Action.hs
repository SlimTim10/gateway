module Config.Rule.Action where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , withObject
  , parseJSON
  )
import qualified Data.HashMap.Strict as HM

import qualified Types.Prop as Prop

type ConfigAction = [ConfigActionElement]

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
