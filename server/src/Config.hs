module Config
  ( Config(..)
  , readConfig
  , ConfigProp(..)
  , ConfigRule(..)
  , ConfigTrigger(..)
  , ConfigAction(..)
  ) where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , decodeFileEither
  , ParseException
  )

import Config.Prop
  ( ConfigProp(..)
  )
import Config.Rule
  ( ConfigRule(..)
  , ConfigTrigger(..)
  , ConfigAction(..)
  )

data Config
  = Config
  { props :: [ConfigProp]
  , rules :: [ConfigRule]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

readConfig :: FilePath -> IO (Either ParseException Config)
readConfig = decodeFileEither
