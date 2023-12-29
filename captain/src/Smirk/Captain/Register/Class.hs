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
  registerMate :: MateId -> Mate -> (Mate -> Bool) -> m (Maybe RegisterError)
  deregisterMate :: MateId -> (Mate -> Bool) -> m (Maybe DeregisterError)

instance MonadRegister SmirkM where
  registerMate mateId mate canOverwrite = do
    Env {mates} <- ask
    liftIO . atomically . stateTVar mates $ \m ->
      case m Map.!? mateId of
        Nothing -> (Nothing, Map.insert mateId mate m)
        Just n | canOverwrite n -> (Nothing, Map.insert mateId mate m)
        Just n | otherwise -> (Just AlreadyRegistered, m)
  deregisterMate mateId isAuthorized = do
    Env {mates} <- ask
    liftIO . atomically . stateTVar mates $ \m ->
      case m Map.!? mateId of
        Nothing -> (Just NotFound, m)
        Just n | isAuthorized n -> (Just Unauthorized, m)
        Just n | otherwise -> (Nothing, Map.delete mateId m)
