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
import System.Console.ANSI
  ( clearScreen
  , saveCursor
  , restoreCursor
  , clearFromCursorToScreenEnd
  )
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
  clearScreen
  saveCursor
  listen serial state rules
  closeSerial serial
  where
    waitForArduino = delaySeconds 2

triggeredRules :: State -> Rules -> Rules
triggeredRules state = filter (checkTrigger state . trigger)

eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM l r x = either l r =<< x

listen :: SerialPort -> State -> Rules -> IO ()
listen serial state rules = do
  eitherM
    discard
    useRawPacket
    (recvRawPacket serial)
  where
    discard e = do
      logWarn $ "Received bad packet: " ++ show e
      listen serial state rules
    updateState state' = do
      state'' <- runRules serial state' rules
      display state'' rules
      listen serial state'' rules
    useRawPacket raw = either
      discard
      updateState
      (handleRawPacket state raw)

display :: State -> Rules -> IO ()
display state rules = do
  restoreCursor
  clearFromCursorToScreenEnd
  restoreCursor
  putStrLn "TRIGGERED RULES"
  print $ triggeredRules state rules
  putStrLn "STATE"
  State.prettyPrint state

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

logWarn :: String -> IO ()
logWarn msg = do
  let msg' = "WARNING: " ++ msg
  putStrLn msg'
