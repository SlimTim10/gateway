module Rule where

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
  )
import Data.Char (toLower)

data Type = Basic | Sequence
  deriving (Show, Eq, Generic, ToJSON)

opts :: Options
opts = defaultOptions { constructorTagModifier = map toLower }

instance FromJSON Type where
  parseJSON = genericParseJSON opts
