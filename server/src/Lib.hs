module Lib
  ( main
  ) where

import           Control.Concurrent             ( threadDelay )
import qualified Data.ByteString.Char8         as BS
import qualified System.Hardware.Serialport    as Serial

main :: IO ()
main = do
  putStrLn "Hello World!"

  let port = "/dev/ttyACM0"
  serial <- Serial.openSerial
    port
    Serial.defaultSerialSettings { Serial.timeout = 10 }

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
