module Smirk.Captain.Register.Class
  ( MonadRegister (..),
    RegisterError (..),
    DeregisterError (..),
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (stateTVar)
import qualified Data.Map.Strict as Map
import Smirk.Captain.Env (Env (..))
import Smirk.Captain.SmirkM
import Smirk.Prelude
import Smirk.Types (AccessToken, Mate (..), MateId)

data RegisterError = AlreadyRegistered

data DeregisterError = NotFound | Unauthorized

class Monad m => MonadRegister m where
  registerMate :: MateId -> Mate -> m (Maybe RegisterError)
  deregisterMate :: MateId -> AccessToken -> m (Maybe DeregisterError)

instance MonadRegister SmirkM where
  registerMate mateId mate = do
    Env {mates} <- ask
    liftIO . atomically . stateTVar mates $ \m ->
      case m Map.!? mateId of
        Nothing -> (Nothing, Map.insert mateId mate m)
        Just _ -> (Just AlreadyRegistered, m)
  deregisterMate mateId accessToken' = do
    Env {mates} <- ask
    liftIO . atomically . stateTVar mates $ \m ->
      case m Map.!? mateId of
        Nothing -> (Just NotFound, m)
        Just n | accessToken n /= accessToken' -> (Just Unauthorized, m)
        Just n | otherwise -> (Nothing, Map.delete mateId m)
