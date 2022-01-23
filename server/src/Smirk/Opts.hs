module Smirk.Opts
  ( Opts(..)
  , parseOpts
  , Cmd(..)
  , ControlCmd(..)
  , module Export
  ) where

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.Warp.Internal
                                               as Warp
import           Options.Applicative
import           System.Hardware.Serialport    as Export
                                                ( SerialPortSettings(..)
                                                , defaultSerialSettings
                                                )

import           Smirk.Control                  ( ControlCmd(..)
                                                , IrSignal(..)
                                                )

pSerialPortSettings :: Parser SerialPortSettings
pSerialPortSettings = do
  timeout <-
    option auto $ long "serial-timeout" <> value 10 <> showDefault <> help
      "Serial timeout in 1/10th seconds"
  pure defaultSerialSettings { timeout = timeout }

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
  , command "send"
  $      (do
           signalProtocol <- option auto $ long "protocol"
           signalValue    <- option auto $ long "value"
           signalBits     <- option auto $ long "bits"
           signalAddress  <- option auto $ long "address"
           pure $ Send IrSignal { .. }
         )
  `info` (fullDesc <> progDesc "Send a code.")
  , command "receive"
  $      pure Receive
  `info` (fullDesc <> progDesc "Get the last received code .")
  ]

pWarpSettings :: Parser Warp.Settings
pWarpSettings = do
  port <-
    option auto
    $  long "port"
    <> short 'p'
    <> help "Webserver port"
    <> showDefault
    <> value 8765
  pure Warp.defaultSettings { Warp.settingsPort = port }

data Cmd
  = Control ControlCmd
  | Serve Warp.Settings

pCmd :: Parser Cmd
pCmd = hsubparser $ mconcat
  [ command "control"
  $      (Control <$> pControlCmd)
  `info` (fullDesc <> progDesc "Control the Smirk Arduino.")
  , command "serve"
  $      (Serve <$> pWarpSettings)
  `info` (fullDesc <> progDesc "Serve a webserver for the Smirk Arduino.")
  ]

data Opts = Opts
  { serialPortSettings :: SerialPortSettings
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
