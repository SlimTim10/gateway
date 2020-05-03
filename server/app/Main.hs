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
-- import Text.Pretty.Simple (pPrint)

import Options
  ( options
  , Options(..)
  )
import Packet (fromRaw)
import ReliableSerial (recvRawPacket)
import qualified Types.Prop as Prop
import Types.Rule (Rule(..))
import Types.Rule.Action (ActionElement(..))
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
  config <- Config.readConfigThrow "test/data/config.yaml"
  delaySeconds 1
  state <- State.fromConfigThrow (Config.props config)
  rules <- Rules.fromConfigThrow state (Config.rules config)
  putStr "Triggered rules: "
  print $ triggeredRules state rules
  putStrLn ""
  putStrLn "Reading cards..."
  putStrLn ""
  state' <- simulate state
  putStr "Triggered rules: "
  let tRules = triggeredRules state' rules
  print tRules
  putStrLn ""
  putStrLn "New state: "
  state'' <- runRules serial state' rules
  print state''
  where
    simulate state = do
      let
        readCards =
          [ ActionElement { address = 1, value = Prop.Int 1 }
          , ActionElement { address = 2, value = Prop.Int 2 }
          ]
      return $ applyAction state readCards
    -- simulate state = do
    --   let bs = B.pack . map chr $ [0x00, 0x00, 0x00, 0x01, 0x01, 0x01]
    --   -- let raw = [0x00, 0x00, 0x00, 0x02, 0x01, 0x02]
    --   case fromRaw bs of
    --     Left x -> error "Invalid packet"
    --     Right packet -> 
