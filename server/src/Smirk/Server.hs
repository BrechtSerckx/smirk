module Smirk.Server (app) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Network.Wai (Application)
import Servant.API.Generic (toServant)
import Servant.Server.Generic (AsServerT, genericServeT)
import Smirk.Api (Routes (..))
import Smirk.Env (Env (..))
import Smirk.Prelude
import Smirk.SmirkM

server :: Routes (AsServerT SmirkM)
server =
  Routes
    { version = getVersion
    }

getVersion :: SmirkM ()
getVersion = return ()

app :: Env -> Application
app env = genericServeT (liftIO . flip runReaderT env . runSmirkM) server
