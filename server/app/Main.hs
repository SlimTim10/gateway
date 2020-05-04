module Main where

import System.Hardware.Serialport
  ( openSerial
  , closeSerial
  , defaultSerialSettings
  -- , SerialPort
  , SerialPortSettings(..)
  , CommSpeed(..)
  )
import Control.Concurrent (threadDelay)
import Options.Applicative (execParser)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr)
-- import Text.Pretty.Simple (pPrint)

import Options
  ( options
  , Options(..)
  )
-- import Packet (fromRaw)
-- import ReliableSerial (recvRawPacket)
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
  s <- openSerial port defaultSerialSettings { commSpeed = baud }
  delaySeconds 3
  -- serialLoop s
  closeSerial s

-- serialLoop :: SerialPort -> IO ()
-- serialLoop s = do
--   eRawPacket <- recvRawPacket s
--   case eRawPacket of
--     Left e -> putStrLn $ "Error: " ++ e
--     Right rawPacket -> do
--       let p = fromRaw rawPacket
--       either putStrLn print p
--   serialLoop s

triggeredRules :: State -> Rules -> Rules
triggeredRules state = filter (checkTrigger state . trigger)

catchEither :: Monad m => Either a b -> (a -> m b) -> m b
catchEither x f = either f return x

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
  
  let bs0 = B.pack . map chr $ [0x00, 0x00, 0x00, 0x01, 0x01, 0x01]
  s1 <- handleRawPacket state bs0 `catchEither` \e -> do
    logWarn $ "Received bad packet: " ++ show e
    return state
  
  let bs1 = B.pack . map chr $ [0x00, 0x00, 0x00, 0x02, 0x01, 0x02]
  state' <- handleRawPacket s1 bs1 `catchEither` \e -> do
    logWarn $ "Received bad packet: " ++ show e
    return s1
  
  putStr "Triggered rules: "
  let tRules = triggeredRules state' rules
  print tRules
  putStrLn ""
  putStrLn "New state: "
  state'' <- runRules serial state' rules
  print state''

logWarn :: String -> IO ()
logWarn msg = do
  let msg' = "WARNING: " ++ msg
  putStrLn msg'
