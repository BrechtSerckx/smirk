{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Smirk
  ( main
  ) where

import qualified Control.Concurrent.Lock       as Lock
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Acquire                   ( mkAcquire
                                                , withAcquire
                                                )
import qualified Data.ByteString.Char8         as BS8
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Control
import           Smirk.M
import           Smirk.Opts

main :: IO ()
main = do
  Opts {..} <- parseOpts
  let acquireSerialPort = mkAcquire
        (Serial.openSerial serialPortPath serialPortSettings)
        Serial.closeSerial
  withAcquire acquireSerialPort $ \serialPort -> do
    serialPortLock <- Lock.new
    let ctx = Ctx { .. }
    flip runM ctx $ case cmd of
      Control controlCmd -> do
        res <- runControlCmd controlCmd
        liftIO $ BS8.putStrLn res
