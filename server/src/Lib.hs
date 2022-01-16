{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
  ( main
  ) where

import           Capability.Reader
import           Capability.Source
import           Control.Concurrent.Lock        ( Lock )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Data.Acquire                   ( mkAcquire
                                                , withAcquire
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import           GHC.Generics                   ( Generic )
import           Options.Applicative
import qualified System.Hardware.Serialport    as Serial

pSerialPortSettings :: Parser Serial.SerialPortSettings
pSerialPortSettings = do
  timeout <-
    option auto $ long "serial-timeout" <> value 10 <> showDefault <> help
      "Serial timeout in 1/10th seconds"
  pure Serial.defaultSerialSettings { Serial.timeout = timeout }

data ControlCmd
  = NoOp
  | Ping
  | Version
  | Add Int
  | Send
  | Receive

pControlCmd :: Parser ControlCmd
pControlCmd = hsubparser $ mconcat
  [ command "no-op" $ pure NoOp `info` (fullDesc <> progDesc "Do nothing.")
  , command "ping" $ pure Ping `info` (fullDesc <> progDesc "Ping pong.")
  , command "version"
  $      pure Version
  `info` (fullDesc <> progDesc "Show Smirk Arduino version.")
  , command "add"
  $      (Add <$> argument auto mempty)
  `info` (fullDesc <> progDesc "Add a number on the Smirk Arduino.")
  , command "send" $ pure Send `info` (fullDesc <> progDesc "Send a code.")
  , command "receive"
  $      pure Receive
  `info` (fullDesc <> progDesc "Get the last received code .")
  ]

data Cmd = Control ControlCmd

pCmd :: Parser Cmd
pCmd = hsubparser $ mconcat
  [ command "control"
    $      (Control <$> pControlCmd)
    `info` (fullDesc <> progDesc "Control the Smirk Arduino.")
  ]

data Opts = Opts
  { serialPortSettings :: Serial.SerialPortSettings
  , serialPortPath     :: FilePath
  , cmd                :: Cmd
  }

pOpts :: Parser Opts
pOpts = do
  serialPortSettings <- pSerialPortSettings
  serialPortPath     <-
    strOption $ long "port" <> short 'p' <> value "/dev/ttyACM0" <> showDefault
  cmd <- pCmd
  pure Opts { .. }

parseOpts :: IO Opts
parseOpts =
  let parserInfo = fullDesc <> progDesc "Smirk controller and server."
  in  execParser $ (pOpts <**> helper) `info` parserInfo

class Monad m => HasSerialPort m where
  withSerialPort :: (Serial.SerialPort -> IO a) -> m a

newtype SerialPortT m a = SerialPortT (m a)
  deriving (Functor, Applicative, Monad) via m
instance
  ( Monad m
  , MonadIO m
  , HasReader "serialPort" Serial.SerialPort m
  , HasReader "serialPortLock" Lock m
  ) => HasSerialPort (SerialPortT m) where
  withSerialPort f = SerialPortT $ do
    lock <- ask @"serialPortLock"
    s    <- ask @"serialPort"
    liftIO . Lock.with lock $ f s

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

data Ctx = Ctx
  { serialPort     :: Serial.SerialPort
  , serialPortLock :: Lock
  }
  deriving stock Generic

newtype M a = M { runM :: Ctx -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT Ctx IO
  deriving ( HasReader "serialPort" Serial.SerialPort
           , HasSource "serialPort" Serial.SerialPort
           )
    via Field "serialPort" "ctx" (MonadReader (ReaderT Ctx IO))
  deriving ( HasReader "serialPortLock" Lock
           , HasSource "serialPortLock" Lock
           )
    via Field "serialPortLock" "ctx" (MonadReader (ReaderT Ctx IO))
  deriving HasSerialPort
  via SerialPortT M

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

main :: IO ()
main = do
  Opts {..} <- parseOpts
  putStrLn "Hello World!"

  let acquireSerialPort = mkAcquire
        (Serial.openSerial serialPortPath serialPortSettings)
        Serial.closeSerial
  withAcquire acquireSerialPort $ \serialPort -> do
    serialPortLock <- Lock.new
    let ctx = Ctx { .. }
    flip runM ctx $ case cmd of
      Control controlCmd -> do
        res <- case controlCmd of
          NoOp    -> expecting resOk $ serialSendRecv cmdNoOp
          Ping    -> expectingWith cmdPing "pong" $ serialSendRecv cmdPing
          Version -> serialSendRecv cmdVersion
          Add i   -> serialSendRecv $ cmdAdd <> BS8.pack (show i)
          Send    -> expecting resOk $ serialSendRecv cmdSend
          Receive -> serialSendRecv cmdReceive
        liftIO $ BS8.putStrLn res

  putStrLn "Goodbye World!"
