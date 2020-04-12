module ReliableSerial
  ( recvPacket
  , sendPacket
  , fletcher16
  , check
  , withCheckBytes
  , withoutCheckBytes
  ) where

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
  ( SerialPort
  , recv
  , send
  )
import Data.Word (Word16)
import Data.Bits
  ( (.|.)
  , (.&.)
  , shiftL
  , shiftR
  )
import Data.Char
  ( chr
  , ord
  )

import Encoding
  ( cobsDecode
  , cobsEncode
  )

-- Packets are surrounded by null bytes, have two check bytes at the end, and are COBS-encoded
recvPacket :: SerialPort -> IO (Either String B.ByteString)
recvPacket s = do
  b <- serialDropWhile (not . valid) s
  bs <- serialTakeWhile valid s
  let raw = cobsDecode (b <> bs)
  let pkt = withoutCheckBytes raw
  return $
    if check raw
    then Right pkt
    else Left "Invalid checksum"
  where
    valid :: B.ByteString -> Bool
    valid x
      = not . any ($ x)
      $
      [ B.null
      , (== excludedByte)
      ]

sendPacket :: SerialPort -> B.ByteString -> IO (Int)
sendPacket s = send s . cobsEncode . withCheckBytes

excludedByte :: B.ByteString
excludedByte = B.singleton $ chr 0x00

check :: B.ByteString -> Bool
check bs = fletcher16 bs == 0x0000

withCheckBytes :: B.ByteString -> B.ByteString
withCheckBytes bs = bs `B.append` checkBytes
  where
    csum = fletcher16 bs
    f0 = csum .&. 0xFF
    f1 = (csum `shiftR` 8) .&. 0xFF
    c0 = 0xFF - ((f0 + f1) `mod` 0xFF)
    c1 = 0xFF - ((f0 + c0) `mod` 0xFF)
    checkBytes = B.pack . map (chr . fromIntegral ) $ [c0, c1]

withoutCheckBytes :: B.ByteString -> B.ByteString
withoutCheckBytes bs = B.take (B.length bs - 2) bs

serialDropWhile :: (B.ByteString -> Bool) -> SerialPort -> IO (B.ByteString)
serialDropWhile p s = do
  b <- recv s 1
  if p b
    then serialDropWhile p s
    else
    do return b

serialTakeWhile :: (B.ByteString -> Bool) -> SerialPort -> IO (B.ByteString)
serialTakeWhile p s = go ""
  where
    go :: B.ByteString -> IO (B.ByteString)
    go bs = do
      b <- recv s 1
      if p b
        then go (bs <> b)
        else return bs

fletcher16 :: B.ByteString -> Word16
fletcher16 = loop 0 0
  where
    loop :: Word16 -> Word16 -> B.ByteString -> Word16
    loop sum1 sum2 bs
      | B.null bs = (sum2 `shiftL` 8) .|. sum1
      | otherwise =
          let
            b = fromIntegral . ord . B.head $ bs
            sum1' = (sum1 + b) `mod` 255 :: Word16
            sum2' = (sum2 + sum1') `mod` 255 :: Word16
          in loop sum1' sum2' (B.tail bs)
