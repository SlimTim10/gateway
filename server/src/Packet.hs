module Packet
  ( Packet(..)
  , RawPacket
  , fromBytes
  , packetFormat
  , slice
  , Format(..)
  , readPacket
  ) where

import GHC.Generics (Generic)
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 (fromStrict)
import qualified Data.Binary.Get as Bin
import Data.Char (ord)

import Lib (readJSON)
import qualified Command as Cmd
import Command (Command)
import qualified Prop

type RawPacket = B.ByteString

data Packet = Packet
  { propAddress :: Int
  , commandID :: Command
  , payload :: Prop.Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Format = Format
  { index :: Int
  , size :: Int
  }
  deriving (Show)

data PacketFormat = PacketFormat
  { propAddress :: Format
  , commandID :: Format
  , payload :: Format
  }
  deriving (Show)

packetFormat :: PacketFormat
packetFormat = PacketFormat
  { propAddress = Format { index = 0, size = 4 }
  , commandID = Format { index = 4, size = 1}
  , payload = Format { index = 5, size = 252 }
  }

minPacketSize :: Int
minPacketSize = n + m
  where
    n = size . (propAddress :: PacketFormat -> Format) $ packetFormat
    m = size . (commandID :: PacketFormat -> Format) $ packetFormat

slice :: Int -> Int -> B.ByteString -> B.ByteString
slice start len = B.take len . B.drop start

getWord32 :: B.ByteString -> Int
getWord32 bs = fromIntegral $ Bin.runGet Bin.getWord32be (fromStrict bs)

getWord16 :: B.ByteString -> Int
getWord16 bs = fromIntegral $ Bin.runGet Bin.getWord16be (fromStrict bs)

getWord8 :: B.ByteString -> Int
getWord8 bs = fromIntegral $ Bin.runGet Bin.getWord8 (fromStrict bs)

getInt :: B.ByteString -> Int
getInt bs = case B.length bs of
  0 -> 0
  1 -> getWord8 bs
  2 -> getWord16 bs
  _ -> getWord32 bs

payloadValue :: Command -> B.ByteString -> Either String Prop.Value
payloadValue Cmd.PayloadInt bs
  | B.null bs = Left "Couldn't match expected single integer with empty payload"
  | B.length bs > 1 =
      Left
      $ "Couldn't match expected single Int with payload of "
      ++ show (B.length bs)
      ++ " bytes"
  | otherwise = Right $ Prop.Int $ getWord8 bs
payloadValue Cmd.PayloadIntList bs
  | B.null bs = Left "Couldn't match expected integer list with empty payload"
  | otherwise = Right $ Prop.IntList ns
  where
    ns = map ord . B.unpack $ bs
payloadValue Cmd.PayloadString bs
  | B.null bs = Left "Couldn't match expected string with empty payload"
  | otherwise = Right $ Prop.String $ B.unpack bs
payloadValue _ _ = Right Prop.Nothing

fromBytes :: RawPacket -> Either String Packet
fromBytes raw
  | B.length raw < minPacketSize = Left "Not enough bytes for packet"
  | otherwise = do
      cmd <- Cmd.fromInt rawCmd
      pld <- payloadValue cmd rawPayload
      return $
        Packet
        { propAddress = addr
        , commandID = cmd
        , payload = pld
        }
  where
    addr = getInt . slice addrIdx addrSize $ raw
    rawCmd = getInt . slice cmdIdx cmdSize $ raw
    rawPayload = slice pldIdx pldSize raw
    addrIdx = index . (propAddress :: PacketFormat -> Format) $ packetFormat
    addrSize = size . (propAddress :: PacketFormat -> Format) $ packetFormat
    cmdIdx = index . (commandID :: PacketFormat -> Format) $ packetFormat
    cmdSize = size . (commandID :: PacketFormat -> Format) $ packetFormat
    pldIdx = index . (payload :: PacketFormat -> Format) $ packetFormat
    pldSize = B.length raw - cmdSize - addrSize

readPacket :: FilePath -> IO (Either String Packet)
readPacket = readJSON
