module Smirk.Control where

import           Control.Monad
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
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

renderControlCmd :: ControlCmd -> BS.ByteString
renderControlCmd = \case
  NoOp    -> BS.singleton 0x00
  Ping    -> BS.singleton 0x01
  Version -> BS.singleton 0x02
  Add i   -> BS.singleton 0x03 <> BS8.pack (show i)
  Send    -> BS.singleton 0x04
  Receive -> BS.singleton 0x05

serialSendRecv :: HasSerialPort m => ControlCmd -> m BS.ByteString
serialSendRecv cmd = withSerialPort $ \s -> do
  void . Serial.send s $ renderControlCmd cmd
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
noOp = expecting resOk $ serialSendRecv NoOp

ping :: HasSerialPort m => m ()
ping = expecting pong $ serialSendRecv Ping where pong = renderControlCmd Ping

version :: HasSerialPort m => m Text
version = Text.decodeUtf8 <$> serialSendRecv Version

add :: HasSerialPort m => Int -> m Int
add i = read . BS8.unpack <$> serialSendRecv (Add i)

send :: HasSerialPort m => m ()
send = expecting resOk $ serialSendRecv Send

receive :: HasSerialPort m => m Int
receive = read . BS8.unpack <$> serialSendRecv Receive
