module Command where

import GHC.Generics (Generic)
import Data.Aeson
  ( FromJSON
  , ToJSON
  )

data Command
  = PayloadInt
  | PayloadIntList
  | PayloadString
  | Ping
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

fromInt :: Int -> Either String Command
fromInt x = case x of
  0x01 -> Right PayloadInt
  0x02 -> Right PayloadIntList
  0x03 -> Right PayloadString
  0x80 -> Right Ping
  _ -> Left "Invalid command ID"
