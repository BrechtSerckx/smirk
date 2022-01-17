module Smirk.Control where

import           Control.Monad
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Effects.SerialPort

data ControlCmd
  = NoOp
  | Ping
  | Version
  | Add Int
  | Send
  | Receive

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

expecting :: Monad m => BS.ByteString -> m BS.ByteString -> m BS.ByteString
expecting expected act = expectingWith expected "OK" act

expectingWith
  :: Monad m
  => BS.ByteString
  -> BS.ByteString
  -> m BS.ByteString
  -> m BS.ByteString
expectingWith expected out act = do
  res <- act
  if res == expected
    then pure out
    else
      error
      $  "expected: 0x"
      <> mconcat (show <$> BS.unpack expected)
      <> ", got: "
      <> mconcat (show <$> BS.unpack res)

runControlCmd :: HasSerialPort m => ControlCmd -> m BS.ByteString
runControlCmd = \case
  NoOp    -> expecting resOk $ serialSendRecv cmdNoOp
  Ping    -> expectingWith cmdPing "pong" $ serialSendRecv cmdPing
  Version -> serialSendRecv cmdVersion
  Add i   -> serialSendRecv $ cmdAdd <> BS8.pack (show i)
  Send    -> expecting resOk $ serialSendRecv cmdSend
  Receive -> serialSendRecv cmdReceive
