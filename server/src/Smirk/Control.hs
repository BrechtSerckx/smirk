module Smirk.Control where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as BSL
import           Data.Data
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import           Debug.Trace
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Effects.SerialPort

data ControlCmd
  = NoOp
  | Ping
  | Version
  | Add Int
  | Send
  | Receive
  deriving stock (Show, Data)
instance ToJSON ControlCmd where
  toJSON cmd =
    let type_ = showConstr $ toConstr cmd
        data_ = case cmd of
          NoOp    -> Nothing
          Ping    -> Nothing
          Version -> Nothing
          Add i   -> Just i
          Send    -> Nothing
          Receive -> Nothing
    in  object ["type" .= type_, "data" .= data_]

resOk :: BS.ByteString
resOk = BS.singleton 0x00

renderControlCmd :: ControlCmd -> BS.ByteString
renderControlCmd = BSL.toStrict . encode

serialSendRecv :: HasSerialPort m => ControlCmd -> m BS.ByteString
serialSendRecv cmd = withSerialPort $ \s -> do
  let bs = renderControlCmd cmd
  traceShowM bs
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
noOp = expecting resOk $ serialSendRecv NoOp

ping :: HasSerialPort m => m ()
ping = expecting pong $ serialSendRecv Ping where pong = BS.singleton 0x01

version :: HasSerialPort m => m Text
version = Text.decodeUtf8 <$> serialSendRecv Version

add :: HasSerialPort m => Int -> m Int
add i = read . BS8.unpack <$> serialSendRecv (Add i)

send :: HasSerialPort m => m ()
send = expecting resOk $ serialSendRecv Send

receive :: HasSerialPort m => m Int
receive = read . BS8.unpack <$> serialSendRecv Receive
