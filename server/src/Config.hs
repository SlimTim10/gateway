module Config
  ( Config(..)
  , ConfigException(..)
  , readConfigThrow
  ) where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , decodeFileThrow
  )
import Control.Exception (Exception)

import Config.Prop (ConfigProp)
import Config.Rule (ConfigRule)

data Config
  = Config
  { props :: [ConfigProp]
  , rules :: [ConfigRule]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ConfigException
  = InvalidTrigger String
  | InvalidAction String
  | PropConflict String [[ConfigProp]]
  deriving (Show, Exception)

readConfigThrow :: FilePath -> IO Config
readConfigThrow = decodeFileThrow
