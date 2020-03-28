module Main where

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
  ( openSerial
  , closeSerial
  , defaultSerialSettings
  , SerialPort(..)
  , SerialPortSettings(..)
  , CommSpeed(..)
  , recv
  , flush
  )
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Data.Semigroup ((<>))
import Options.Applicative (execParser)

import Options (options, Options(..))

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
  case line of
    Just bs ->  do
      let cs = B.unpack bs
      mapM_ (printf "0x%X ") cs
      putStrLn ""
      putStrLn cs
      loop newline s
    Nothing -> loop newline s

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