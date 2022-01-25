{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Smirk.Server
  ( runSmirkServer
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Acquire                   ( withAcquire )
import           Data.Aeson.Extra.SingObject    ( SingObject(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.RequestLogger
                                               as Wai
import           Servant.API
import qualified Servant.Server                as Servant

import qualified Smirk.Control                 as Control
import           Smirk.Control                  ( IrSignal )
import           Smirk.Effects.SerialPort
import           Smirk.M

-- * General endpoints

-- brittany-disable-next-binding
type VersionEP
  =  "api"
  :> "version"
  :> Get '[JSON] (SingObject "version" Text)

versionHandler :: HasSerialPort m => Servant.ServerT VersionEP m
versionHandler = SingObject <$> Control.version

-- brittany-disable-next-binding
type PingEP
  =  "api"
  :> "ping"
  :> Post '[JSON] (SingObject "response" Text)

pingHandler :: HasSerialPort m => Servant.ServerT PingEP m
pingHandler = SingObject <$> Control.ping

-- * Raw IrSignal endpoints

-- brittany-disable-next-binding
type GetLatestIrSignalEP
  =  "api"
  :> "signal"
  :> "latest-received"
  :> Get '[JSON] IrSignal

getLatestIrSignalHandler
  :: HasSerialPort m => Servant.ServerT GetLatestIrSignalEP m
getLatestIrSignalHandler = Control.receive

-- brittany-disable-next-binding
type SendIrSignalEP
  =  "api"
  :> "signal"
  :> "send"
  :> ReqBody '[JSON] IrSignal
  :> PostNoContent

sendIrSignalHandler :: HasSerialPort m => Servant.ServerT SendIrSignalEP m
sendIrSignalHandler irSignal = Control.send irSignal $> NoContent


-- * Full API

-- brittany-disable-next-binding
type SmirkApi
  = (  VersionEP
  :<|> PingEP
  :<|> GetLatestIrSignalEP
  :<|> SendIrSignalEP
    )

pSmirkApi :: Proxy SmirkApi
pSmirkApi = Proxy

smirkServer
  :: (HasSerialPort m, HasState "signalMap" (Map Text IrSignal) m)
  => Servant.ServerT SmirkApi m
smirkServer =
  versionHandler
    :<|> pingHandler
    :<|> getLatestIrSignalHandler
    :<|> sendIrSignalHandler

mToHandler :: MkCtx -> forall a . M a -> Servant.Handler a
mToHandler (acquireSerialPort, mkCtx) act =
  liftIO . withAcquire acquireSerialPort $ \serialPort ->
    act `runM` mkCtx serialPort

runSmirkServer :: Warp.Settings -> MkCtx -> IO ()
runSmirkServer warpSettings mkCtx = do
  putStrLn "Starting webserver"
  Warp.runSettings warpSettings
    . Wai.logStdoutDev
    . Servant.serve pSmirkApi
    . Servant.hoistServer pSmirkApi (mToHandler mkCtx)
    $ smirkServer
