{-# LANGUAGE RankNTypes #-}
module Smirk.Server
  ( runSmirkServer
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Acquire                   ( withAcquire )
import           Data.Proxy                     ( Proxy(..) )
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant.API
import qualified Servant.Server                as Servant

import           Smirk.M

type SmirkApi = EmptyAPI

pSmirkApi :: Proxy SmirkApi
pSmirkApi = Proxy

smirkServer :: Servant.ServerT SmirkApi m
smirkServer = Servant.emptyServer

mToHandler :: MkCtx -> forall a . M a -> Servant.Handler a
mToHandler (acquireSerialPort, mkCtx) act =
  liftIO . withAcquire acquireSerialPort $ \serialPort ->
    act `runM` mkCtx serialPort

runSmirkServer :: Warp.Settings -> MkCtx -> IO ()
runSmirkServer warpSettings mkCtx =
  Warp.runSettings warpSettings
    . Servant.serve pSmirkApi
    . Servant.hoistServer pSmirkApi (mToHandler mkCtx)
    $ smirkServer
