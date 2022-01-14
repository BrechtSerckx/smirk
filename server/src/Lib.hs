module Lib
  ( main
  ) where

import           Control.Concurrent             ( threadDelay )
import qualified Data.ByteString.Char8         as BS
import           Options.Applicative
import qualified System.Hardware.Serialport    as Serial

pSerialPortSettings :: Parser Serial.SerialPortSettings
pSerialPortSettings = do
  timeout <-
    option auto $ long "serial-timeout" <> value 10 <> showDefault <> help
      "Serial timeout in 1/10th seconds"
  pure Serial.defaultSerialSettings { Serial.timeout = timeout }

data Opts = Opts
  { serialPortSettings :: Serial.SerialPortSettings
  , serialPortPath     :: FilePath
  }

pOpts :: Parser Opts
pOpts = do
  serialPortSettings <- pSerialPortSettings
  serialPortPath     <-
    strOption $ long "port" <> short 'p' <> value "/dev/ttyACM0" <> showDefault
  pure Opts { .. }

parseOpts :: IO Opts
parseOpts =
  let parserInfo = fullDesc <> progDesc "Smirk controller and server"
  in  execParser $ (pOpts <**> helper) `info` parserInfo

main :: IO ()
main = do
  Opts {..} <- parseOpts
  putStrLn "Hello World!"

  serial <- Serial.openSerial serialPortPath serialPortSettings

  putStrLn "No-op"
  Serial.send serial "0"
  threadDelay 1000000

  putStrLn "Ping"
  Serial.send serial "1"
  res <- Serial.recv serial 10
  BS.putStrLn res
  threadDelay 1000000

  putStrLn "Version"
  Serial.send serial "2"
  res <- Serial.recv serial 10
  BS.putStrLn res
  threadDelay 1000000

  putStrLn "Add to 42"
  Serial.send serial "342"
  res <- Serial.recv serial 1000
  BS.putStrLn res
  threadDelay 1000000

  putStrLn "Send"
  Serial.send serial "4"
  threadDelay 1000000

  putStrLn "Receive"
  Serial.send serial "5"
  res <- Serial.recv serial 10
  BS.putStrLn res
  threadDelay 1000000

  Serial.closeSerial serial

  putStrLn "Goodbye World!"
