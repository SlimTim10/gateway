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

import Config.Rule.Trigger (ConfigTrigger)
import Config.Rule.Action (ConfigAction)
import Types.Rule
  ( Type
  , Description
  )

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

