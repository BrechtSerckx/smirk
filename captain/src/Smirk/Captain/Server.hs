module Smirk.Captain.Server (app) where

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
import Smirk.Captain.Api (Routes (..))
import Smirk.Captain.Env (Env (..))
import Smirk.Captain.Register.Class (MonadRegister)
import qualified Smirk.Captain.Register.Server as Register
import Smirk.Captain.SmirkM
import Smirk.Prelude

server ::
  ( Monad m,
    MonadRandom m,
    MonadRegister m,
    MonadThrow m,
    MonadLogger m
  ) =>
  Routes (AsServerT m)
server =
  Routes
    { version = getVersion,
      registerApi = toServant Register.server
    }

getVersion :: Monad m => m ()
getVersion = return ()

app :: Env -> Application
app env = genericServeT (liftIO . flip runReaderT env . runSmirkM) server
