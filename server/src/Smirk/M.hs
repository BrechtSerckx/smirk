{-# LANGUAGE DataKinds #-}
module Smirk.M
  ( Ctx(..)
  , MkCtx
  , M(..)
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
import           Control.Concurrent.STM.TVar    ( TVar )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Data.Acquire                   ( Acquire )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Control                  ( IrSignal )
import           Smirk.Effects.SerialPort

data Ctx = Ctx
  { serialPort     :: Serial.SerialPort
  , serialPortLock :: Lock
  , signalMap      :: TVar (Map Text IrSignal)
  }
  deriving stock Generic
type MkCtx = (Acquire Serial.SerialPort, Serial.SerialPort -> Ctx)

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
