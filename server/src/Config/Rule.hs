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
import qualified Data.HashMap.Strict as HM

import qualified Config.Prop as Prop

data Type = Basic | Sequence
  deriving (Show, Eq, Generic, ToJSON)

type Description = Maybe Text

instance FromJSON Type where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toLower }

data Rule
  = Rule
  { ruleType :: Type
  , description :: Description
  , trigger :: [Trigger]
  , action :: [Action]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Rule where
  parseJSON = withObject "rule" $ \o -> do
    ruleType <- o .: "type"
    description <- o .:? "description"
    trigger <- o .: "trigger"
    action <- o .: "action"
    return $ Rule ruleType description trigger action

data Trigger
  = Trigger
  { name :: Prop.Name
  , value :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Trigger where
  parseJSON = withObject "name-value" $ \o -> do
    let [(name, value')] = HM.toList o
    value <- parseJSON value'
    return $ Trigger name value

data Action
  = Action
  { name :: Prop.Name
  , value :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Action where
  parseJSON = withObject "name-value" $ \o -> do
    let [(name, value')] = HM.toList o
    value <- parseJSON value'
    return $ Action name value
