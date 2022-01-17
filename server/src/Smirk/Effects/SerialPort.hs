{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Smirk.Effects.SerialPort
  ( HasSerialPort(..)
  , SerialPortT(..)
  ) where

import           Capability.Reader              ( HasReader
                                                , ask
                                                )
import           Control.Concurrent.Lock        ( Lock )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified System.Hardware.Serialport    as Serial

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
