module Main where

import System.Hardware.Serialport
  ( openSerial
  , closeSerial
  , defaultSerialSettings
  , SerialPort
  , SerialPortSettings(..)
  , CommSpeed(..)
  )
import Control.Concurrent (threadDelay)
import Options.Applicative (execParser)
import Text.Pretty.Simple (pPrint)

import Options
  ( options
  , Options(..)
  )
import Packet (fromRaw)
import ReliableSerial (recvRawPacket)
import qualified Types.Prop as Prop
import Types.Rule (Rule(..))
import qualified Types.Rule as Rule
import qualified Config
import State (State)
import Server
  ( checkTrigger
  , applyAction
  , runRules
  )
import qualified State
import Rules (Rules)
import qualified Rules

main :: IO ()
main = run =<< execParser options

delaySeconds :: Int -> IO ()
delaySeconds = threadDelay . secondsToMicro
  where
    secondsToMicro = (* 1000) . (* 1000)

run :: Options -> IO ()
run (Options port baud) = do
  s <- openSerial port defaultSerialSettings { commSpeed = baud }
  delaySeconds 3
  serialLoop s
  closeSerial s

serialLoop :: SerialPort -> IO ()
serialLoop s = do
  eRawPacket <- recvRawPacket s
  case eRawPacket of
    Left e -> putStrLn $ "Error: " ++ e
    Right rawPacket -> do
      let p = fromRaw rawPacket
      either putStrLn print p
  serialLoop s

triggeredRules :: State -> Rules -> Rules
triggeredRules state = filter (checkTrigger state . trigger)

dev :: IO ()
dev = do
  let port = "COM19"
  let baud = CS115200
  serial <- openSerial port defaultSerialSettings { commSpeed = baud }
  delaySeconds 3
  result <- Config.readConfig "test/data/config.yaml"
  case result of
    Left err -> print err
    Right config -> do
      let state = State.fromConfig (Config.props config)
      case Rules.fromConfig state (Config.rules config) of
        Left err -> print err
        Right rules -> do
          putStr "Before: "
          print $ triggeredRules state rules
          let
            readCards =
              [ Rule.ActionElement { propKey = 1, value = Prop.Int 1 }
              , Rule.ActionElement { propKey = 2, value = Prop.Int 2 }
              ]
          let state' = applyAction state readCards
          putStr "After: "
          let tRules = triggeredRules state' rules
          print tRules
          putStrLn "New state: "
          state'' <- runRules serial state' rules
          pPrint state''
