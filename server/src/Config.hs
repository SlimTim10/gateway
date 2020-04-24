module Config where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , (.:)
  , (.:?)
  , withObject
  , parseJSON
  , decodeFileEither
  , ParseException
  )
import Data.Word (Word32)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import qualified Prop
import qualified Rule

data ConfigProp
  = ConfigProp
  { name :: String
  , description :: Maybe String
  , address :: Word32
  , defaultValue :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ConfigProp where
  parseJSON = withObject "prop" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    address <- o .: "address"
    defaultValue <- o .: "default-value"
    return $ ConfigProp name description address defaultValue

data NameValue
  = NameValue
  { name :: String
  , value :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON NameValue where
  parseJSON = withObject "name-value" $ \o -> do
    let [(name', value')] = HM.toList o
    let name = T.unpack name'
    value <- parseJSON value'
    return $ NameValue name value

data ConfigRule
  = ConfigRule
  { ruleType :: Rule.Type
  , description :: Maybe String
  , trigger :: [NameValue]
  , action :: [NameValue]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ConfigRule where
  parseJSON = withObject "rule" $ \o -> do
    ruleType <- o .: "type"
    description <- o .:? "description"
    trigger <- o .: "trigger"
    action <- o .: "action"
    return $ ConfigRule ruleType description trigger action

data Config
  = Config
  { props :: [ConfigProp]
  , rules :: [ConfigRule]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

readConfig :: FilePath -> IO (Either ParseException Config)
readConfig = decodeFileEither
