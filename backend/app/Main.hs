module Main where

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
  ( openSerial
  , closeSerial
  , defaultSerialSettings
  , SerialPort(..)
  , SerialPortSettings(..)
  , CommSpeed(..)
  , recv
  , flush
  )
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Data.Semigroup ((<>))

secondsToMicro :: Int -> Int
secondsToMicro = (* 1000) . (* 1000)

main :: IO ()
main = do
  let port = "COM19"          -- Windows
  -- let port = "/dev/ttyUSB0"  -- Linux
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  threadDelay $ secondsToMicro 3
  loop s
  closeSerial s

loop :: SerialPort -> IO ()
loop s = do
  line <- recvLine s
  case line of
    Just bs ->  do
      let cs = B.unpack bs
      mapM_ (printf "0x%X ") cs
      putStrLn ""
      putStrLn cs
      loop s
    Nothing -> loop s

recvLine :: SerialPort -> IO (Maybe B.ByteString)
recvLine = recvLine' ""

recvLine' :: B.ByteString -> SerialPort -> IO (Maybe B.ByteString)
recvLine' bs s = case lineSuffix `B.stripSuffix` bs of
  Nothing -> next
  line -> return line
  where
    lineSuffix = "\r\n"
    next = do
      b <- recv s 1
      if B.null b
        then return Nothing
        else recvLine' (bs <> b) s
