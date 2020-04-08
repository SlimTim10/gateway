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
import Control.Monad (forM_)

import Options (options, Options(..))

import Packet (fromASCIIBytes)

import Packet (Packet(..))
import Data.Yaml (encode)

secondsToMicro :: Int -> Int
secondsToMicro = (* 1000) . (* 1000)

main :: IO ()
main = run =<< execParser options

run :: Options -> IO ()
run (Options port baud newline) = do
  s <- openSerial port defaultSerialSettings { commSpeed = baud }
  wait
  loop newline s
  closeSerial s
  where
    wait = threadDelay $ secondsToMicro 3

loop :: String -> SerialPort -> IO ()
loop newline s = do
  line <- recvLine (B.pack newline) s
  forM_ line $ \bs -> do
    either putStrLn print (fromASCIIBytes bs)
  loop newline s

recvLine :: B.ByteString -> SerialPort -> IO (Maybe B.ByteString)
recvLine = recvLine' ""

recvLine' :: B.ByteString -> B.ByteString -> SerialPort -> IO (Maybe B.ByteString)
recvLine' bs newline s = case newline `B.stripSuffix` bs of
  Nothing -> next
  line -> return line
  where
    next = do
      b <- recv s 1
      if B.null b
        then return Nothing
        else recvLine' (bs <> b) newline s

test :: IO ()
test = do
  let p = Packet { propAddress = 1, payload = [3, 2, 1] }
  print $ encode p
