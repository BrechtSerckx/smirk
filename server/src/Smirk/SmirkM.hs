module Smirk.SmirkM
  ( SmirkM,
    runSmirkM,
    ask,
    liftIO,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Smirk.Env (Env (..))
import Smirk.Prelude

newtype SmirkM a = SmirkM {runSmirkM :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)
