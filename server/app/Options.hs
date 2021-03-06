module Options (options, Options(..)) where

import Options.Applicative
import System.Hardware.Serialport (CommSpeed(..))

data Options = Options
  { port :: String
  , baud :: CommSpeed
  }

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
  ( long "port"
    <> short 'p'
    <> metavar "PORT"
    <> help "Serial port, e.g. on Windows: COM19, on Linux: /dev/ttyUSB0"
  )
  <*> option parseBaud
  ( long "baud"
    <> short 'b'
    <> metavar "BAUD-RATE"
    <> showDefault
    <> value CS115200
    <> help "Baud rate"
  )

options :: ParserInfo Options
options = info (parseOptions <**> helper)
  $ fullDesc
  <> progDesc "Run backend"
  <> header "backend"

parseBaud :: ReadM CommSpeed
parseBaud = eitherReader $ \s -> case s of
  "110" -> Right CS110
  "300" -> Right CS300
  "600" -> Right CS600
  "1200" -> Right CS1200
  "2400" -> Right CS2400
  "4800" -> Right CS4800
  "9600" -> Right CS9600
  "19200" -> Right CS19200
  "38400" -> Right CS38400
  "57600" -> Right CS57600
  "115200" -> Right CS115200
  _ -> Left "Invalid baud rate"
