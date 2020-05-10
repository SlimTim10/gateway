module Types.Packet
  ( Packet(..)
  , RawPacket
  , PacketException(..)
  , fromRaw
  , toRaw
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
import Data.Word (Word8, Word32)
import Data.ByteString.FastBuilder
  ( toStrictByteString
  , word32BE
  , word8
  )
import Control.Exception (Exception)
import Data.List (intercalate)
import Numeric (showHex)

import Lib (readJSON)
import qualified Types.Command as Cmd
import Types.Command (Command)
import qualified Types.Prop as Prop

data PacketException
  = PacketTooSmall
  | UnexpectedEmptyPayload String
  | UnexpectedPayload String B.ByteString
  | InvalidCommand RawPacket
  | InvalidPropAddress Int
  | InvalidChecksum RawPacket
  deriving (Exception)

instance Show PacketException where
  show PacketTooSmall = "Too small"
  show (UnexpectedEmptyPayload expect) = "Couldn't match expected " ++ expect ++ " with empty payload"
  show (UnexpectedPayload expect pld) = "Couldn't match expected " ++ expect ++ " with payload " ++ showHexBytes pld
  show (InvalidCommand raw) = "Invalid command ID in " ++ showHexBytes raw
  show (InvalidPropAddress n) = "Prop at address " ++ showHexNumber n ++ " does not exist"
  show (InvalidChecksum raw) = "Invalid checksum in " ++ showHexBytes raw

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

getWord32 :: B.ByteString -> Word32
getWord32 bs = fromIntegral $ Bin.runGet Bin.getWord32be (fromStrict bs)

getWord8 :: B.ByteString -> Word8
getWord8 bs = fromIntegral $ Bin.runGet Bin.getWord8 (fromStrict bs)

payloadValue :: Command -> B.ByteString -> Either PacketException Prop.Value
payloadValue Cmd.PayloadInt bs
  | B.null bs = Left $ UnexpectedEmptyPayload "single integer"
  | B.length bs > 1 = Left $ UnexpectedPayload "single integer" bs
  | otherwise = Right $ Prop.Int $ getWord8 bs
payloadValue Cmd.PayloadIntList bs
  | B.null bs = Left $ UnexpectedEmptyPayload "integer list"
  | otherwise = Right $ Prop.IntList ns
  where
    ns = map (fromIntegral . ord) . B.unpack $ bs
payloadValue Cmd.PayloadString bs
  | B.null bs = Left $ UnexpectedEmptyPayload "string"
  | otherwise = Right $ Prop.String $ B.unpack bs
payloadValue _ _ = Right Prop.Nothing

fromRaw :: RawPacket -> Either PacketException Packet
fromRaw raw
  | B.length raw < minPacketSize = Left PacketTooSmall
  | otherwise = do
      case Cmd.fromInt rawCmd of
        Nothing -> Left $ InvalidCommand raw
        Just cmd -> do
          pld <- payloadValue cmd rawPayload
          return $
            Packet
            { propAddress = addr
            , commandID = cmd
            , payload = pld
            }
  where
    addr = fromIntegral . getWord32 . slice addrIdx addrSize $ raw
    rawCmd = getWord8 . slice cmdIdx cmdSize $ raw
    rawPayload = slice pldIdx pldSize raw
    addrIdx = index . (propAddress :: PacketFormat -> Format) $ packetFormat
    addrSize = size . (propAddress :: PacketFormat -> Format) $ packetFormat
    cmdIdx = index . (commandID :: PacketFormat -> Format) $ packetFormat
    cmdSize = size . (commandID :: PacketFormat -> Format) $ packetFormat
    pldIdx = index . (payload :: PacketFormat -> Format) $ packetFormat
    pldSize = B.length raw - cmdSize - addrSize

toRaw :: Packet -> RawPacket
toRaw
  Packet
  { propAddress = addr
  , commandID = cmd
  , payload = pld
  }
  = B.concat
  $
  [ toStrictByteString . word32BE . fromIntegral $ addr
  , toStrictByteString . word8 . Cmd.toInt $ cmd
  , Prop.rawValue pld
  ]

readPacket :: FilePath -> IO (Either String Packet)
readPacket = readJSON

showHexBytes :: B.ByteString -> String
showHexBytes bs = "[" ++ showHexBytes' bs ++ "]"

showHexBytes' :: B.ByteString -> String
showHexBytes'
  = intercalate ", "
  . map (showHexNumber . ord)
  . B.unpack

showHexNumber :: Int -> String
showHexNumber n = "0x" ++ prefix ++ showHex n ""
  where
    prefix
      | n < 0x10 = "0"
      | otherwise = ""
