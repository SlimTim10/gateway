module Packet
  ( Packet(..)
  , fromBytes
  , fromASCIIBytes
  , getPayloadLength
  , packetFormat
  , headerFormat
  , payloadFormat
  , slice
  , Format(..), PacketFormat(..), HeaderFormat(..)
  , readPacket
  ) where

import Prelude hiding (length, index)
import GHC.Generics (Generic)
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (fromStrict)
import qualified Data.Binary.Get as Bin
import Data.Char (chr, ord)
import Data.Maybe (fromJust, isNothing)

import Lib (readJSON)

data Packet = Packet
  { propAddress :: Int
  , payload :: [Int]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Format = Format
  { index :: Int
  , length :: Int
  }
  deriving (Show)

data PacketFormat = PacketFormat
  { header :: HeaderFormat
  , payload :: Format
  }
  deriving (Show)

data HeaderFormat = HeaderFormat
  { indexH :: Int
  , lengthH :: Int
  , propAddress :: Format
  , payloadLength :: Format
  }
  deriving (Show)

packetFormat = PacketFormat
  { header = headerFormat
  , payload = payloadFormat
  }

headerFormat = HeaderFormat
  { indexH = 0
  , lengthH = 5
  , propAddress = Format
    { index = 0
    , length = 4
    }
  , payloadLength = Format
    { index = 4
    , length = 1
    }
  }

payloadFormat = Format
  { index = lengthH headerFormat
  , length = 255
  }

slice :: Int -> Int -> ByteString -> ByteString
slice start length = B.take length . B.drop start

getWord32 :: ByteString -> Int
getWord32 bs = fromIntegral $ Bin.runGet Bin.getWord32be (fromStrict bs)

getWord16 :: ByteString -> Int
getWord16 bs = fromIntegral $ Bin.runGet Bin.getWord16be (fromStrict bs)

getWord8 :: ByteString -> Int
getWord8 bs = fromIntegral $ Bin.runGet Bin.getWord8 (fromStrict bs)

getInt :: ByteString -> Int
getInt bs = case B.length bs of
  0 -> 0
  1 -> getWord8 bs
  2 -> getWord16 bs
  _ -> getWord32 bs

type RawPacket = ByteString

getPayloadLength :: RawPacket -> Int
getPayloadLength raw = getInt plRaw
  where
    plRaw = slice plIdx plLen hdr
    plIdx = index . payloadLength $ headerFormat
    plLen = length . payloadLength $ headerFormat
    hdr = slice (indexH headerFormat) (lengthH headerFormat) raw

fromBytes :: RawPacket -> Either String Packet
fromBytes raw
  | B.length raw < lengthH headerFormat = Left "Not enough bytes for header"
  | B.length raw /= lengthH headerFormat + pLen = Left "Actual payload does not match length specified in header"
  | otherwise = Right Packet
    { propAddress = addr
    , payload = map ord . B.unpack . slice (index payloadFormat) pLen $ raw
    }
  where
    pLen = getPayloadLength raw
    addr = getInt . slice addrIdx addrLen $ raw
    addrIdx = index . (propAddress :: HeaderFormat -> Format) $ headerFormat
    addrLen = length . (propAddress :: HeaderFormat -> Format) $ headerFormat

fromASCIIBytes :: RawPacket -> Either String Packet
fromASCIIBytes = fromBytes . B.map fromASCII
  where fromASCII b = chr $ max (ord b - 0x30) 0x00

readPacket :: FilePath -> IO (Either String Packet)
readPacket = readJSON
