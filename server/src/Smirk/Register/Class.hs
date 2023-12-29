module Smirk.Register.Class
  ( MonadRegister (..),
    RegisterError (..),
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (stateTVar)
import qualified Data.Map.Strict as Map
import Smirk.Env (Env (..))
import Smirk.Prelude
import Smirk.SmirkM
import Smirk.Types (Node, NodeId)

data RegisterError = AlreadyRegistered

class Monad m => MonadRegister m where
  registerNode :: NodeId -> Node -> m (Maybe RegisterError)

instance MonadRegister SmirkM where
  registerNode nodeId node = do
    Env {nodes} <- ask
    liftIO . atomically . stateTVar nodes $ \m ->
      case m Map.!? nodeId of
        Nothing -> (Nothing, Map.insert nodeId node m)
        Just _ -> (Just AlreadyRegistered, m)
