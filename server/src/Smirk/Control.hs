module Smirk.Control where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as BSL
import           Data.Data
import           Data.Text                      ( Text )
import           Data.Text.Read
import           Data.Word
import           Debug.Trace
import           GHC.Generics                   ( Generic )
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Effects.SerialPort

data SignalProtocol
  = Unknown
  | NEC
  | Sony
  | RC5
  | RC6
  | PanasonicOld
  | JVC
  | NECX
  | Samsung36
  | GiCable
  | DirecTV
  | RCMM
  | CYKM
  deriving stock (Generic, Show, Read, Bounded, Enum, Data)
  deriving anyclass (FromJSON, ToJSON)

data IrSignal = IrSignal
  { signalProtocol :: SignalProtocol
  , signalValue    :: Word32
  , signalBits     :: Word8
  , signalAddress  :: Word16
  }
  deriving stock (Show, Data)
instance FromJSON IrSignal where
  parseJSON = withObject "IrSignal" $ \o -> do
    signalProtocol <- o .: "protocol"
    signalValue    <- either error fst . hexadecimal <$> o .: "value"
    signalBits     <- o .: "bits"
    signalAddress  <- either error fst . hexadecimal <$> o .: "address"
    pure IrSignal { .. }
instance ToJSON IrSignal where
  toJSON IrSignal {..} = object
    [ "protocol" .= signalProtocol
    , "value" .= signalValue
    , "bits" .= signalBits
    , "address" .= signalAddress
    ]

newtype InternalIrSignal = InternalIrSignal { unInternalIrSignal :: IrSignal }
instance FromJSON InternalIrSignal where
  parseJSON = withObject "InternalIrSignal" $ \o -> do
    signalProtocol <- toEnum <$> o .: "protocolNum"
    signalValue    <- o .: "value"
    signalBits     <- o .: "bits"
    signalAddress  <- o .: "address"
    pure $ InternalIrSignal IrSignal { .. }
instance ToJSON InternalIrSignal where
  toJSON (InternalIrSignal IrSignal {..}) = object
    [ "protocolNum" .= fromEnum signalProtocol
    , "value" .= signalValue
    , "bits" .= signalBits
    , "address" .= signalAddress
    ]

data ControlCmd
  = NoOp
  | Ping
  | Version
  | Send IrSignal
  | Receive
  deriving stock (Show, Data)

instance ToJSON ControlCmd where
  toJSON cmd =
    let type_ = showConstr $ toConstr cmd
        data_ = case cmd of
          NoOp    -> Nothing
          Ping    -> Nothing
          Version -> Nothing
          Send s  -> Just . toJSON $ InternalIrSignal s
          Receive -> Nothing
    in  object ["cmd" .= type_, "data" .= data_]

data SerialResponse a
  = SerialSuccess a
  | SerialFailure String

instance FromJSON a => FromJSON (SerialResponse a) where
  parseJSON = withObject "SerialResponse" $ \o -> o .: "success" >>= \case
    False -> SerialFailure <$> o .: "data"
    True  -> SerialSuccess <$> o .: "data"

data Ok = Ok
  deriving stock (Show, Eq)

instance FromJSON Ok where
  parseJSON _ = pure Ok

renderControlCmd :: ControlCmd -> BS.ByteString
renderControlCmd = BSL.toStrict . encode

serialSendRecv :: (HasSerialPort m, FromJSON a) => ControlCmd -> m a
serialSendRecv cmd = withSerialPort $ \s -> do
  let request = renderControlCmd cmd
  traceM $ "Request: " <> BS8.unpack request
  void $ Serial.send s request
  response <- recvAll s 100
  traceM $ "Response: " <> BS8.unpack response
  let eResp = eitherDecode $ BSL.fromStrict response
  case eResp of
    Left  e                 -> error e
    Right (SerialFailure e) -> error e
    Right (SerialSuccess a) -> pure a
 where
  recvAll s timeout =
    let go acc = do
          res <- Serial.recv s timeout
          case BS.stripSuffix "\n" res of
            Nothing   -> go $ acc <> res
            Just res' -> pure $ acc <> res'
    in  go ""


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

send :: HasSerialPort m => IrSignal -> m ()
send = expecting Ok . serialSendRecv . Send

receive :: HasSerialPort m => m IrSignal
receive = unInternalIrSignal <$> serialSendRecv Receive
