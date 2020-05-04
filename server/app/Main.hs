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
import qualified Data.ByteString.Char8 as B
import Data.Char (chr)
import System.Console.ANSI (clearScreen)
-- import Text.Pretty.Simple (pPrint)

import Options
  ( options
  , Options(..)
  )
-- import Packet (fromRaw)
import ReliableSerial (recvRawPacket)
-- import qualified Types.Prop as Prop
import Types.Rule (Rule(..))
-- import Types.Rule.Action (ActionElement(..))
import qualified Config
import State (State)
import Server
  ( checkTrigger
  -- , applyAction
  , runRules
  , handleRawPacket
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
  serial <- openSerial port defaultSerialSettings { commSpeed = baud }
  waitForArduino
  config <- Config.readConfigThrow "test/data/config.yaml"
  state <- State.fromConfigThrow (Config.props config)
  rules <- Rules.fromConfigThrow state (Config.rules config)
  listen serial state rules
  closeSerial serial
  where
    waitForArduino = delaySeconds 2

triggeredRules :: State -> Rules -> Rules
triggeredRules state = filter (checkTrigger state . trigger)

listen :: SerialPort -> State -> Rules -> IO ()
listen serial state rules = do
  eRaw <- recvRawPacket serial
  case eRaw of
    Left e1 -> do
      logWarn $ "Received bad packet: " ++ show e1
      continue
    Right raw -> do
      case handleRawPacket state raw of
        Left e2 -> do
          logWarn $ "Received bad packet: " ++ show e2
          continue
        Right state' -> do
          state'' <- runRules serial state' rules
          display state'' rules
          listen serial state'' rules
  where
    continue = listen serial state rules

display :: State -> Rules -> IO ()
display state rules = do
  clearScreen
  putStrLn "TRIGGERED RULES"
  print $ triggeredRules state rules
  putStrLn "STATE"
  print state

dev :: IO ()
dev = do
  let port = "COM19"
  let baud = CS115200
  serial <- openSerial port defaultSerialSettings { commSpeed = baud }
  config <- Config.readConfigThrow "test/data/config.yaml"
  delaySeconds 1
  state <- State.fromConfigThrow (Config.props config)
  rules <- Rules.fromConfigThrow state (Config.rules config)
  listen serial state rules

catchEither :: Monad m => Either a b -> (a -> m b) -> m b
catchEither x f = either f return x

catchEitherM :: Monad m => m (Either a b) -> (a -> m b) -> m b
catchEitherM mx f = do
  x <- mx
  catchEither x f

dev2 :: IO ()
dev2 = do
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
  
  let bs0 = B.pack . map chr $ [0x00, 0x00, 0x00, 0x01, 0x01, 0x01]
  s1 <- handleRawPacket state bs0 `catchEither` \e -> do
    logWarn $ "Received bad packet: " ++ show e
    return state
  
  let bs1 = B.pack . map chr $ [0x00, 0x00, 0x00, 0x02, 0x01, 0x02]
  state' <- handleRawPacket s1 bs1 `catchEither` \e -> do
    logWarn $ "Received bad packet: " ++ show e
    return s1
  
  putStr "Triggered rules: "
  print $ triggeredRules state rules
  
  putStrLn ""
  putStrLn "New state: "
  state'' <- runRules serial state' rules
  print state''

logWarn :: String -> IO ()
logWarn msg = do
  let msg' = "WARNING: " ++ msg
  putStrLn msg'
