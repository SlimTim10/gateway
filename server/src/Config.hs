module Config
  ( Config(..)
  , readConfig
  -- , validate
  ) where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , decodeFileEither
  , ParseException
  )

import Config.Prop (ConfigProp)
import Config.Rule (ConfigRule)

data Config
  = Config
  { props :: [ConfigProp]
  , rules :: [ConfigRule]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

readConfig :: FilePath -> IO (Either ParseException Config)
readConfig = decodeFileEither

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
