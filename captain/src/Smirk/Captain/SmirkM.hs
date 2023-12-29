module Smirk.Captain.SmirkM
  ( SmirkM,
    runSmirkM,
    ask,
    liftIO,
    runClient,
    MonadMateClient,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
  ( MonadLogger (..),
    runStderrLoggingT,
  )
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    ask,
  )
import qualified Servant.Client as Servant
  ( BaseUrl,
    ClientEnv (..),
    ClientM,
    runClientM,
  )
import Smirk.Captain.Env (Env (..))
import Smirk.Prelude

newtype SmirkM a = SmirkM {runSmirkM :: ReaderT Env IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadThrow,
      MonadRandom
    )

instance MonadLogger SmirkM where
  monadLoggerLog loc src lvl msg =
    runStderrLoggingT $ monadLoggerLog loc src lvl msg

class MonadMateClient m where
  runClient :: Servant.BaseUrl -> Servant.ClientM a -> m a

instance MonadMateClient SmirkM where
  runClient baseUrl act = do
    Env {servantClientEnv} <- ask
    eRes <-
      liftIO $
        Servant.runClientM act servantClientEnv {Servant.baseUrl = baseUrl}
    case eRes of
      Left e -> throwM e
      Right a -> return a
