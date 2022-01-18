{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Smirk.Server
  ( runSmirkServer
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Acquire                   ( withAcquire )
import           Data.Aeson
import           Data.Proxy                     ( Proxy(..) )
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.RequestLogger
                                               as Wai
import           Servant.API
import qualified Servant.Server                as Servant

import           Smirk.Control
import           Smirk.M

newtype IrSignal = IrSignal {signal  :: Int}
instance FromJSON IrSignal where
  parseJSON = withObject "IrSignal" $ \o -> do
    signal <- o .: "signal"
    pure IrSignal { .. }
instance ToJSON IrSignal where
  toJSON IrSignal {..} = object ["signal" .= signal]

-- brittany-disable-next-binding
type SmirkApi
  =   "api"
  :>  "signal"
  :>  (  -- get last received infrared signal
         Get '[JSON] IrSignal
    :<|> -- send infrared signal
         ReqBody '[JSON] IrSignal
      :> Post '[JSON] ()
      )

pSmirkApi :: Proxy SmirkApi
pSmirkApi = Proxy

smirkServer :: Monad m => Servant.ServerT SmirkApi m
smirkServer = getLatestIrSignal :<|> sendIrSignal
 where
  getLatestIrSignal = pure IrSignal { signal = 0 }
  sendIrSignal _ = pure ()

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
