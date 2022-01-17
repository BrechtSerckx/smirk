{-# LANGUAGE DataKinds #-}
module Smirk.M
  ( Ctx(..)
  , M(..)
  ) where

import           Capability.Accessors           ( Field(..) )
import           Capability.Reader              ( HasReader
                                                , MonadReader(..)
                                                )
import           Capability.Source              ( HasSource )
import           Control.Concurrent.Lock        ( Lock )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           GHC.Generics                   ( Generic )
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Effects.SerialPort

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
