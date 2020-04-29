module Command where

import GHC.Generics (Generic)
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Word (Word8)

data Command
  = PayloadInt
  | PayloadIntList
  | PayloadString
  | Ping
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

fromInt :: Word8 -> Either String Command
fromInt x = case x of
  0x01 -> Right PayloadInt
  0x02 -> Right PayloadIntList
  0x03 -> Right PayloadString
  0x80 -> Right Ping
  _ -> Left "Invalid command ID"

toInt :: Command -> Word8
toInt cmd = case cmd of
  PayloadInt -> 0x01
  PayloadIntList -> 0x02
  PayloadString -> 0x03
  Ping -> 0x80
