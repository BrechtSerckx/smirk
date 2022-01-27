{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Smirk.M
  ( Ctx(..)
  , M(..)
  , RunWithCtx(..)
  , mkRunWithCtx
  , withAcquire
  ) where

import           Capability.Accessors           ( Field(..) )
import           Capability.Reader              ( HasReader
                                                , MonadReader(..)
                                                )
import           Capability.ReaderTVar          ( ReaderTVar(..) )
import           Capability.Sink                ( HasSink )
import           Capability.Source              ( HasSource )
import           Capability.State               ( HasState )
import           Control.Concurrent.Lock        ( Lock )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Concurrent.STM         ( TVar )
import qualified Control.Concurrent.STM        as Stm
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Data.Acquire                   ( 
                                                mkAcquire
                                                , withAcquire
                                                )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Yaml
import           GHC.Generics                   ( Generic )
import qualified System.Directory              as Directory
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Control                  ( IrSignal )
import           Smirk.Effects.SerialPort
import           Smirk.Opts

data Ctx = Ctx
  { serialPort     :: Serial.SerialPort
  , serialPortLock :: Lock
  , signalMapPath  :: FilePath
  , signalMap      :: TVar (Map Text IrSignal)
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
  deriving ( HasSource "signalMap" (Map Text IrSignal)
           , HasSink "signalMap" (Map Text IrSignal)
           , HasState "signalMap" (Map Text IrSignal)
           ) via ReaderTVar (Field "signalMap" "ctx" (MonadReader (ReaderT Ctx IO)))
  deriving ( HasReader "signalMapPath" FilePath
           , HasSource "signalMapPath" FilePath
           )
    via Field "signalMapPath" "ctx" (MonadReader (ReaderT Ctx IO))

data RunWithCtx where
  RunWithCtx :: (forall a . (Ctx -> IO a) -> IO a) -> RunWithCtx

mkRunWithCtx :: Opts -> IO RunWithCtx
mkRunWithCtx Opts {..} = do
  let acquireSerialPort = mkAcquire
        (Serial.openSerial serialPortPath serialPortSettings)
        Serial.closeSerial
  serialPortLock <- Lock.new
  initSignalMap  <- Directory.doesFileExist signalMapPath >>= \case
    False -> mempty
    True  -> Yaml.decodeFileThrow signalMapPath
  signalMap <- Stm.newTVarIO initSignalMap
  pure $ RunWithCtx (\f -> withAcquire @IO acquireSerialPort $ \serialPort -> f Ctx { .. })
