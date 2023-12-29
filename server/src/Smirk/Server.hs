module Smirk.Server (app) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    runReaderT,
  )
import Network.Wai (Application)
import Servant.API.Generic (toServant)
import Servant.Server.Generic
  ( AsServerT,
    genericServeT,
  )
import Smirk.Api (Routes (..))
import Smirk.Env (Env (..))
import Smirk.Prelude
import Smirk.Register.Class (MonadRegister)
import qualified Smirk.Register.Server
import Smirk.SmirkM

server ::
  ( Monad m,
    MonadRandom m,
    MonadRegister m,
    MonadThrow m,
    MonadIO m
  ) =>
  Routes (AsServerT m)
server =
  Routes
    { version = getVersion,
      registerApi = toServant Smirk.Register.Server.server
    }

getVersion :: Monad m => m ()
getVersion = return ()

app :: Env -> Application
app env = genericServeT (liftIO . flip runReaderT env . runSmirkM) server
