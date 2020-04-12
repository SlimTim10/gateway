module Main where

import System.Hardware.Serialport
  ( openSerial
  , closeSerial
  , defaultSerialSettings
  , SerialPort
  , SerialPortSettings(..)
  )
import Control.Concurrent
  ( threadDelay
  )
import Options.Applicative
  ( execParser
  )
import qualified Data.Yaml as Yaml

import Options
  ( options
  , Options(..)
  )
import Packet
  ( fromBytes
  , Packet(..)
  )
import ReliableSerial
  ( recvRawPacket
  )

secondsToMicro :: Int -> Int
secondsToMicro = (* 1000) . (* 1000)

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run (Options port baud) = do
  s <- openSerial port defaultSerialSettings { commSpeed = baud }
  wait
  serialLoop s
  closeSerial s
  where
    wait = threadDelay $ secondsToMicro 3

serialLoop :: SerialPort -> IO ()
serialLoop s = do
  eRawPacket <- recvRawPacket s
  case eRawPacket of
    Left e -> putStrLn $ "Error: " ++ e
    Right rawPacket -> do
      let p = fromBytes rawPacket
      either putStrLn print p
  serialLoop s

test :: IO ()
test = do
  let p = Packet { propAddress = 1, payload = [3, 2, 1] }
  print $ Yaml.encode p
