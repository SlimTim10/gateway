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
  )
import Control.Concurrent (threadWaitRead, threadDelay)

secondsToMicro :: Int -> Int
secondsToMicro = (* 1000) . (* 1000)

main :: IO ()
main = do
  let port = "COM19"          -- Windows
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  loop s
  closeSerial s

loop :: SerialPort -> IO ()
loop s = do
  bs <- recv s 60
  if B.length bs > 0
    then print bs >> loop s
    else loop s
