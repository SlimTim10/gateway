module Config
  ( Config(..)
  , readConfig
  , Prop(..)
  , Rule(..)
  , NameValue(..)
  ) where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  , decodeFileEither
  , ParseException
  )

import Config.Prop (Prop(..))
import Config.Rule (Rule(..))
import Config.NameValue (NameValue(..))

data Config
  = Config
  { props :: [Prop]
  , rules :: [Rule]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

readConfig :: FilePath -> IO (Either ParseException Config)
readConfig = decodeFileEither
