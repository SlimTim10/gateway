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

import Options
  ( options
  , Options(..)
  )
import Packet
  ( fromBytes
  )
import ReliableSerial
  ( recvRawPacket
  )
-- import qualified Command as Cmd
-- import qualified Types.Prop as Prop
-- import qualified Types.Rule as Rule
-- import qualified Config
-- import qualified State
-- import qualified Rules

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

-- test :: IO ()
-- test = do
--   result <- Config.readConfig "test/data/config.yaml"
--   let (Right config) = result
--   let (Right state) = State.fromConfig (Config.props config)
--   let (Right rules) = Rules.fromConfig state (Config.rules config)
--   let firstTrigger = Rule.trigger . head $ rules
--   print $ all (State.checkTrigger state) firstTrigger
