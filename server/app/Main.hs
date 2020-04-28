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
import qualified Types.Rule as Rule
import Config
  ( Config
  )
import qualified Config
import State
  ( State
  )
import qualified State
import qualified Rules

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

dev :: IO ()
dev = do
  result <- Config.readConfig "test/data/config.yaml"
  case result of
    Left err -> print err
    Right config -> do
      let state = State.fromConfig (Config.props config)
      print state
      print config
      -- Left err -> print err
      -- Right ft -> print $ State.checkTrigger state ft

-- activeActions :: Config -> [Action]
-- activeActions config = do
--   state <- State.fromConfig (Config.props config)
--   rules <- Rules.fromConfig state (Config.rules config)
  

firstTrigger :: State -> Config -> Either String Rule.Trigger
firstTrigger state config = do
  rules <- Rules.fromConfig state (Config.rules config)
  return $ Rule.trigger . head $ rules
