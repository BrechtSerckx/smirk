module Lib
  ( main
  ) where

import           Control.Concurrent.Lock        ( Lock )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Monad
import           Data.Acquire                   ( mkAcquire
                                                , withAcquire
                                                )
import qualified Data.ByteString.Char8         as BS
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

withSerialPort
  :: Serial.SerialPort -> Lock -> (Serial.SerialPort -> IO a) -> IO a
withSerialPort s lock f = Lock.with lock $ f s

serialSend :: Serial.SerialPort -> Lock -> BS.ByteString -> IO ()
serialSend s' l bs = void . withSerialPort s' l $ \s -> Serial.send s bs

serialSendRecv
  :: Serial.SerialPort -> Lock -> BS.ByteString -> IO BS.ByteString
serialSendRecv s' l bs = withSerialPort s' l $ \s -> do
  void $ Serial.send s bs
  Serial.recv s 10

main :: IO ()
main = do
  Opts {..} <- parseOpts
  putStrLn "Hello World!"

  let acquireSerialPort = mkAcquire
        (Serial.openSerial serialPortPath serialPortSettings)
        Serial.closeSerial
  serialPortLock <- Lock.new
  withAcquire acquireSerialPort $ \serialPort -> case cmd of
    Control controlCmd -> case controlCmd of
      NoOp -> serialSend serialPort serialPortLock "0"

      Ping -> do
        res <- serialSendRecv serialPort serialPortLock "1"
        BS.putStrLn res

      Version -> do
        res <- serialSendRecv serialPort serialPortLock "2"
        BS.putStrLn res

      Add i -> do
        res <- serialSendRecv serialPort serialPortLock $ "3" <> BS.pack
          (show i)
        BS.putStrLn res

      Send    -> serialSend serialPort serialPortLock "4"

      Receive -> do
        res <- serialSendRecv serialPort serialPortLock "5"
        BS.putStrLn res

  putStrLn "Goodbye World!"
