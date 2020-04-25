module Config.Prop where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  )
import Data.Aeson
  ( parseJSON
  , genericParseJSON
  , defaultOptions
  , Options(..)
  , SumEncoding(..)
  )
import Data.Word (Word8)

data Value
  = Int Word8
  | IntList [Word8]
  | String String
  | Nothing
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Value where
  parseJSON = genericParseJSON
    defaultOptions { sumEncoding = UntaggedValue }
