module Config
  ( Config(..)
  , ConfigException(..)
  , readConfig
  -- , validate
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
  deriving (Show, Exception)

readConfig :: FilePath -> IO Config
readConfig = decodeFileThrow

-- validate :: Config -> Either String Config
-- validate = undefined

-- uniquePropNames :: Config -> Either String Config
-- uniquePropNames = undefined

-- uniquePropAddresses :: Config -> Either String Config
-- uniquePropAddresses = undefined

-- noRedundantTriggers :: Config -> Either String Config
-- noRedundantTriggers = undefined

-- validPropNamesInTriggers :: Config -> Either String Config
-- validPropNamesInTriggers = undefined
