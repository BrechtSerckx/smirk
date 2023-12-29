module Smirk.Captain.Server (app) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    runReaderT,
  )
import qualified Data.Map.Strict as Map
import Network.Wai (Application)
import Servant.API.Generic (toServant)
import Servant.Server (ServerError (..), err404)
import Servant.Server.Generic
  ( AsServerT,
    genericServeT,
  )
import Smirk.Captain.Api (Routes (..))
import Smirk.Captain.Env (Env (..))
import Smirk.Captain.MateStore (MonadMateStore)
import qualified Smirk.Captain.Register.Server as Register
import Smirk.Captain.SmirkM
import qualified Smirk.Mate.Client as Mate
import Smirk.Prelude
import Smirk.Types

server ::
  ( Monad m,
    MonadRandom m,
    MonadMateStore m,
    MonadThrow m,
    MonadLogger m,
    MonadReader Env m,
    MonadIO m,
    MonadMateClient m
  ) =>
  Routes (AsServerT m)
server =
  Routes
    { version = getVersion,
      registerApi = toServant Register.server,
      send = sendSignal
    }

getVersion :: Monad m => m ()
getVersion = return ()

-- FIXME: do this properly
sendSignal ::
  (MonadIO m, MonadReader Env m, MonadThrow m, MonadMateClient m) =>
  MateId ->
  () ->
  m ()
sendSignal mateId payload = do
  Env {mates} <- ask
  mMate <- Map.lookup mateId <$> liftIO (readTVarIO mates)
  mate <- maybe (throwM err404) pure mMate
  runClient undefined $ Mate.doSend Nothing payload

app :: Env -> Application
app env = genericServeT (liftIO . flip runReaderT env . runSmirkM) server
