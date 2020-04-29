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
-- import Text.Pretty.Simple (pPrint)

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
import qualified Types.Prop as Prop
import Types.Rule
  ( Rule(..)
  )
import qualified Types.Rule as Rule
import qualified Config
import State
  ( State
  )
import Server
  ( checkTrigger
  , applyAction
  )
import qualified State
import Rules
  ( Rules
  )
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
          let state'' = applyAction state' (Rule.action . head $ tRules)
          print state''

triggeredRules :: State -> Rules -> Rules
triggeredRules state = filter (triggeredRule state)

triggeredRule :: State -> Rule -> Bool
triggeredRule state (Rule { trigger = t }) = checkTrigger state t
