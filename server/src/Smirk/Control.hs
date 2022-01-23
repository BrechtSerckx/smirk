module Smirk.Control where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Data.Data
import           Data.Text                      ( Text )
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

data SerialResponse a
  = SerialSuccess a
  | SerialFailure String

instance FromJSON a => FromJSON (SerialResponse a) where
  parseJSON = withObject "SerialResponse" $ \o -> o .: "type" >>= \case
    ("Failure" :: Text) -> SerialFailure <$> o .: "data"
    ("Success" :: Text) -> SerialSuccess <$> o .: "data"
    t                   -> fail $ "Unrecognized 'type': " <> show t

data Ok = Ok
  deriving stock (Show, Eq)

instance FromJSON Ok where
  parseJSON _ = pure Ok

renderControlCmd :: ControlCmd -> BS.ByteString
renderControlCmd = BSL.toStrict . encode

serialSendRecv :: (HasSerialPort m, FromJSON a) => ControlCmd -> m a
serialSendRecv cmd = withSerialPort $ \s -> do
  let request = renderControlCmd cmd
  traceShowM request
  void $ Serial.send s request
  response <- Serial.recv s 2000
  traceShowM response
  let eResp = eitherDecode $ BSL.fromStrict response
  case eResp of
    Left  e                 -> error e
    Right (SerialFailure e) -> error e
    Right (SerialSuccess a) -> pure a

expecting :: forall a m . (Monad m, Show a, Eq a) => a -> m a -> m ()
expecting expected act = do
  res <- act
  unless (res == expected)
    .  error
    $  "expected: 0x"
    <> show expected
    <> ", got: "
    <> show res

noOp :: HasSerialPort m => m ()
noOp = expecting Ok $ serialSendRecv NoOp

ping :: HasSerialPort m => m ()
ping = expecting ("pong" :: Text) $ serialSendRecv Ping

version :: HasSerialPort m => m Text
version = serialSendRecv Version

add :: HasSerialPort m => Int -> m Int
add = serialSendRecv . Add

send :: HasSerialPort m => m ()
send = expecting Ok $ serialSendRecv Send

receive :: HasSerialPort m => m Int
receive = serialSendRecv Receive
