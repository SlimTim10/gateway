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

import Types.Prop
  ( Name
  , Description
  , Address
  , Value
  )

data ConfigProp
  = ConfigProp
  { name :: Name
  , description :: Description
  , address :: Address
  , defaultValue :: Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ConfigProp where
  parseJSON = withObject "prop" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    address <- o .: "address"
    defaultValue <- o .: "default-value"
    return $ ConfigProp name description address defaultValue
