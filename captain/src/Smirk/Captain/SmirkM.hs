module Smirk.Captain.SmirkM
  ( SmirkM,
    runSmirkM,
    ask,
    liftIO,
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
