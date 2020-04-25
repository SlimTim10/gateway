module Config.Prop where

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
  , SumEncoding(..)
  )
import Data.Word (Word8, Word32)
import Data.Text (Text)

data Value
  = Int Word8
  | IntList [Word8]
  | String String
  | Nothing
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Value where
  parseJSON = genericParseJSON
    defaultOptions { sumEncoding = UntaggedValue }

data Prop
  = Prop
  { name :: Text
  , description :: Maybe Text
  , address :: Word32
  , defaultValue :: Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Prop where
  parseJSON = withObject "prop" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    address <- o .: "address"
    defaultValue <- o .: "default-value"
    return $ Prop name description address defaultValue

