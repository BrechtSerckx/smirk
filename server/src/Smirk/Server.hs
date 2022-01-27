{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Smirk.Server
  ( runSmirkServer
  ) where

import           Capability.Reader              ( HasReader )
import qualified Capability.Reader             as Reader
import           Capability.State               ( HasState )
import qualified Capability.State              as State
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson.Extra.SingObject    ( SingObject(..) )
import           Data.Functor
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Yaml
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

-- * Named IrSignal endpoints

-- brittany-disable-next-binding
type GetNamedIrSignalsEP
  =  "api"
  :> "named-signal"
  :> Get '[JSON] (Map Text IrSignal)

getNamedIrSignalsHandler
  :: HasState "signalMap" (Map Text IrSignal) m
  => Servant.ServerT GetNamedIrSignalsEP m
getNamedIrSignalsHandler = State.get @"signalMap"

-- brittany-disable-next-binding
type RefreshNamedIrSignalsEP
  =  "api"
  :> "named-signal"
  :> "refresh"
  :> PostNoContent

refreshNamedIrSignalsHandler
  :: ( HasState "signalMap" (Map Text IrSignal) m
     , HasReader "signalMapPath" FilePath m
     , MonadIO m
     )
  => Servant.ServerT RefreshNamedIrSignalsEP m
refreshNamedIrSignalsHandler = do
  signalMapPath <- Reader.ask @"signalMapPath"
  signalMap     <- Yaml.decodeFileThrow signalMapPath
  State.put @"signalMap" signalMap
  pure NoContent

-- brittany-disable-next-binding
type PutLatestIrSignalEP
  =  "api"
  :> "named-signal"
  :> "save-latest-received"
  :> QueryParam' '[Required] "name" Text
  :> PostNoContent

putLatestIrSignalHandler
  :: ( HasSerialPort m
     , HasState "signalMap" (Map Text IrSignal) m
     , HasReader "signalMapPath" FilePath m
     , MonadIO m
     )
  => Servant.ServerT PutLatestIrSignalEP m
putLatestIrSignalHandler name = do
  signal <- Control.receive
  State.modify' @"signalMap" $ Map.insert name signal
  writeSignalMap
  pure NoContent

-- brittany-disable-next-binding
type PutNamedIrSignal
  =  "api"
  :> "named-signal"
  :> Capture "name" Text
  :> ReqBody '[JSON] IrSignal
  :> PutNoContent

putNamedIrSignalHandler
  :: ( HasState "signalMap" (Map Text IrSignal) m
     , HasReader "signalMapPath" FilePath m
     , MonadIO m
     )
  => Servant.ServerT PutNamedIrSignal m
putNamedIrSignalHandler name signal = do
  State.modify' @"signalMap" $ Map.insert name signal
  writeSignalMap
  pure NoContent

-- brittany-disable-next-binding
type SendNamedIrSignal
  =  "api"
  :> "named-signal"
  :> Capture "name" Text
  :> "send"
  :> PostNoContent

sendNamedIrSignalHandler
  :: (HasSerialPort m, HasState "signalMap" (Map Text IrSignal) m)
  => Servant.ServerT SendNamedIrSignal m
sendNamedIrSignalHandler name =
  State.gets @"signalMap" (Map.lookup name) >>= \case
    Just irSignal -> Control.send irSignal $> NoContent
    Nothing       -> error "No signal with that name."

-- brittany-disable-next-binding
type DeleteNamedIrSignal
  =  "api"
  :> "named-signal"
  :> Capture "name" Text
  :> DeleteNoContent

deleteNamedIrSignalHandler
  :: ( HasState "signalMap" (Map Text IrSignal) m
     , HasReader "signalMapPath" FilePath m
     , MonadIO m
     )
  => Servant.ServerT DeleteNamedIrSignal m
deleteNamedIrSignalHandler name = do
  State.modify' @"signalMap" $ Map.delete name
  writeSignalMap
  pure NoContent

writeSignalMap
  :: ( HasState "signalMap" (Map Text IrSignal) m
     , HasReader "signalMapPath" FilePath m
     , MonadIO m
     )
  => m ()
writeSignalMap = do
  signalMap     <- State.get @"signalMap"
  signalMapPath <- Reader.ask @"signalMapPath"
  liftIO $ Yaml.encodeFile signalMapPath signalMap

-- * Full API

-- brittany-disable-next-binding
type SmirkApi
  = (  VersionEP
  :<|> PingEP
  :<|> GetLatestIrSignalEP
  :<|> SendIrSignalEP
  :<|> GetNamedIrSignalsEP
  :<|> RefreshNamedIrSignalsEP
  :<|> PutLatestIrSignalEP
  :<|> PutNamedIrSignal
  :<|> SendNamedIrSignal
  :<|> DeleteNamedIrSignal
    )

pSmirkApi :: Proxy SmirkApi
pSmirkApi = Proxy

smirkServer
  :: ( HasSerialPort m
     , HasState "signalMap" (Map Text IrSignal) m
     , HasReader "signalMapPath" FilePath m
     , MonadIO m
     )
  => Servant.ServerT SmirkApi m
smirkServer =
  versionHandler
    :<|> pingHandler
    :<|> getLatestIrSignalHandler
    :<|> sendIrSignalHandler
    :<|> getNamedIrSignalsHandler
    :<|> refreshNamedIrSignalsHandler
    :<|> putLatestIrSignalHandler
    :<|> putNamedIrSignalHandler
    :<|> sendNamedIrSignalHandler
    :<|> deleteNamedIrSignalHandler

mToHandler :: RunWithCtx -> forall a . M a -> Servant.Handler a
mToHandler (RunWithCtx runWithCtx) = 
  liftIO . runWithCtx . runM

runSmirkServer :: Warp.Settings -> RunWithCtx -> IO ()
runSmirkServer warpSettings runWithCtx = do
  putStrLn "Starting webserver"
  Warp.runSettings warpSettings
    . Wai.logStdoutDev
    . Servant.serve pSmirkApi
    . Servant.hoistServer pSmirkApi (mToHandler runWithCtx)
    $ smirkServer
