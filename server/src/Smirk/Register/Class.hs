module Smirk.Register.Class
  ( MonadRegister (..),
    RegisterError (..),
    DeregisterError (..),
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (stateTVar)
import qualified Data.Map.Strict as Map
import Smirk.Env (Env (..))
import Smirk.Prelude
import Smirk.SmirkM
import Smirk.Types (AccessToken, Node (..), NodeId)

data RegisterError = AlreadyRegistered

data DeregisterError = NotFound | Unauthorized

class Monad m => MonadRegister m where
  registerNode :: NodeId -> Node -> m (Maybe RegisterError)
  deregisterNode :: NodeId -> AccessToken -> m (Maybe DeregisterError)

instance MonadRegister SmirkM where
  registerNode nodeId node = do
    Env {nodes} <- ask
    liftIO . atomically . stateTVar nodes $ \m ->
      case m Map.!? nodeId of
        Nothing -> (Nothing, Map.insert nodeId node m)
        Just _ -> (Just AlreadyRegistered, m)
  deregisterNode nodeId accessToken' = do
    Env {nodes} <- ask
    liftIO . atomically . stateTVar nodes $ \m ->
      case m Map.!? nodeId of
        Nothing -> (Just NotFound, m)
        Just n | accessToken n /= accessToken' -> (Just Unauthorized, m)
        Just n | otherwise -> (Nothing, Map.delete nodeId m)
