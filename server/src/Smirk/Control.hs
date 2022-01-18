module Smirk.Control where

import           Control.Monad
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Effects.SerialPort

resOk :: BS.ByteString
resOk = BS.singleton 0x00

cmdNoOp :: BS.ByteString
cmdNoOp = BS.singleton 0x00

cmdPing :: BS.ByteString
cmdPing = BS.singleton 0x01

cmdVersion :: BS.ByteString
cmdVersion = BS.singleton 0x02

cmdAdd :: BS.ByteString
cmdAdd = BS.singleton 0x03

cmdSend :: BS.ByteString
cmdSend = BS.singleton 0x04

cmdReceive :: BS.ByteString
cmdReceive = BS.singleton 0x05

serialSendRecv :: HasSerialPort m => BS.ByteString -> m BS.ByteString
serialSendRecv bs = withSerialPort $ \s -> do
  void $ Serial.send s bs
  Serial.recv s 100

expecting :: Monad m => BS.ByteString -> m BS.ByteString -> m ()
expecting expected act = do
  res <- act
  unless (res == expected)
    .  error
    $  "expected: 0x"
    <> mconcat (show <$> BS.unpack expected)
    <> ", got: "
    <> mconcat (show <$> BS.unpack res)

noOp :: HasSerialPort m => m ()
noOp = expecting resOk $ serialSendRecv cmdNoOp

ping :: HasSerialPort m => m ()
ping = expecting cmdPing $ serialSendRecv cmdPing

version :: HasSerialPort m => m Text
version = Text.decodeUtf8 <$> serialSendRecv cmdVersion

add :: HasSerialPort m => Int -> m Int
add i =
  let i' = BS8.pack $ show i
  in  read . BS8.unpack <$> serialSendRecv (cmdAdd <> i')

send :: HasSerialPort m => m ()
send = expecting resOk $ serialSendRecv cmdSend

receive :: HasSerialPort m => m Int
receive = read . BS8.unpack <$> serialSendRecv cmdReceive
