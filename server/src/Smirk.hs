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
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified System.Hardware.Serialport    as Serial

import           Smirk.Control
import           Smirk.M
import           Smirk.Opts
import           Smirk.Server                   ( runSmirkServer )

main :: IO ()
main = do
  Opts {..} <- parseOpts
  let acquireSerialPort = mkAcquire
        (Serial.openSerial serialPortPath serialPortSettings)
        Serial.closeSerial
  serialPortLock <- Lock.new
  case cmd of
    Control controlCmd -> withAcquire acquireSerialPort $ \serialPort -> do
      let ctx = Ctx { .. }
      flip runM ctx $ do
        res <- case controlCmd of
          Ping    -> ping >> pure "pong"
          Version -> version
          Send s  -> send s >> pure "OK"
          Receive -> Text.pack . show <$> receive
        liftIO $ Text.putStrLn res
    Serve warpSettings ->
      let mkCtx serialPort = Ctx { .. }
      in  runSmirkServer warpSettings (acquireSerialPort, mkCtx)
