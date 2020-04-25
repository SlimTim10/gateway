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
import Data.Aeson
  ( genericParseJSON
  , defaultOptions
  , Options(..)
  )
import Data.Char (toLower)
import Data.Text (Text)

import Config.NameValue (NameValue)

data Type = Basic | Sequence
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Type where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toLower }

data Rule
  = Rule
  { ruleType :: Type
  , description :: Maybe Text
  , trigger :: [NameValue]
  , action :: [NameValue]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Rule where
  parseJSON = withObject "rule" $ \o -> do
    ruleType <- o .: "type"
    description <- o .:? "description"
    trigger <- o .: "trigger"
    action <- o .: "action"
    return $ Rule ruleType description trigger action
