module Prop where

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

data Value
  = Int Int
  | IntList [Int]
  | String String
  | Nothing
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Value where
  parseJSON = genericParseJSON
    defaultOptions { sumEncoding = ObjectWithSingleField }
