{-# LANGUAGE RankNTypes #-}
module Smirk.Server
  ( runSmirkServer
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Acquire                   ( Acquire
                                                , withAcquire
                                                )
import           Data.Proxy                     ( Proxy(..) )
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant.API
import qualified Servant.Server                as Servant
import qualified System.Hardware.Serialport    as Serial

import           Smirk.M

type SmirkApi = EmptyAPI

pSmirkApi :: Proxy SmirkApi
pSmirkApi = Proxy

smirkServer :: Servant.ServerT SmirkApi m
smirkServer = Servant.emptyServer

mToHandler
  :: Acquire Serial.SerialPort
  -> (Serial.SerialPort -> Ctx)
  -> forall a . M a -> Servant.Handler a
mToHandler acquireSerialPort mkCtx act =
  liftIO . withAcquire acquireSerialPort $ \serialPort ->
    act `runM` mkCtx serialPort

runSmirkServer
  :: Acquire Serial.SerialPort -> (Serial.SerialPort -> Ctx) -> IO ()
runSmirkServer acquireSerialPort mkCtx =
  Warp.run 8765
    . Servant.serve pSmirkApi
    . Servant.hoistServer pSmirkApi (mToHandler acquireSerialPort mkCtx)
    $ smirkServer
