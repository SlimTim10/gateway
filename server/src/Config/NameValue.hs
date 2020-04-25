module Config.NameValue where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , withObject
  , parseJSON
  )
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

import qualified Config.Prop as Prop

data NameValue
  = NameValue
  { name :: Text
  , value :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON NameValue where
  parseJSON = withObject "name-value" $ \o -> do
    let [(name, value')] = HM.toList o
    value <- parseJSON value'
    return $ NameValue name value
