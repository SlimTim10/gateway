module Prop where

import GHC.Generics (Generic)
import Data.Yaml
  ( FromJSON
  , ToJSON
  )

data Value
  = Int Int
  | IntList [Int]
  | String String
  | Nothing
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


