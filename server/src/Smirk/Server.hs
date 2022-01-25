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

import           Smirk.Control
import           Smirk.Effects.SerialPort
import           Smirk.M



-- brittany-disable-next-binding
type SmirkApi = "api" :>
    (  "version" :> Get '[JSON] (SingObject "version" Text)
  :<|> "ping" :> Post '[JSON] (SingObject "response" Text)
       -- get last received infrared signal
  :<|> "signal" :> Get '[JSON] IrSignal
       -- send infrared signal
  :<|> "signal" :> "send" :> ReqBody '[JSON] IrSignal :> Post '[JSON] ()
    )

pSmirkApi :: Proxy SmirkApi
pSmirkApi = Proxy

smirkServer :: HasSerialPort m => Servant.ServerT SmirkApi m
smirkServer = getVersion :<|> sendPing :<|> getLatestIrSignal :<|> sendIrSignal
 where
  getVersion        = SingObject <$> version
  sendPing          = SingObject <$> ping
  getLatestIrSignal = receive
  sendIrSignal      = send

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
