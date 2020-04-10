module Main where

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
  ( openSerial
  , closeSerial
  , defaultSerialSettings
  , SerialPort
  , SerialPortSettings(..)
  , recv
  )
import Control.Concurrent (threadDelay)
import Options.Applicative (execParser)

import Options (options, Options(..))

import Packet (fromBytes)

import Packet (Packet(..))
import qualified Data.Yaml as Yaml
import Encoding (decodeCOBS, cobsBoundary)

secondsToMicro :: Int -> Int
secondsToMicro = (* 1000) . (* 1000)

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run (Options port baud) = do
  s <- openSerial port defaultSerialSettings { commSpeed = baud }
  wait
  loop s
  closeSerial s
  where
    wait = threadDelay $ secondsToMicro 3

loop :: SerialPort -> IO ()
loop s = do
  rawPacket <- recvPacket s
  either putStrLn print $ (fromBytes . decodeCOBS $ rawPacket)
  loop s

recvPacket :: SerialPort -> IO (B.ByteString)
recvPacket s = do
  let valid x = not $ B.null x || x == cobsBoundary
  b <- serialDropWhile (not . valid) s
  bs <- serialTakeWhile valid s
  return (b <> bs)

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

test :: IO ()
test = do
  let p = Packet { propAddress = 1, payload = [3, 2, 1] }
  print $ Yaml.encode p
