module Smirk.Captain.MateStore
  ( MonadMateStore (..),
    InsertError (..),
    RemoveError (..),
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (stateTVar)
import qualified Data.Map.Strict as Map
import Smirk.Captain.Env (Env (..))
import Smirk.Captain.SmirkM
import Smirk.Prelude
import Smirk.Types (AccessToken, Mate (..), MateId)

data InsertError = AlreadyPresent

data RemoveError = NotFound | Forbidden

class Monad m => MonadMateStore m where
  insert :: MateId -> Mate -> (Mate -> Bool) -> m (Maybe InsertError)
  remove :: MateId -> (Mate -> Bool) -> m (Maybe RemoveError)

instance MonadMateStore SmirkM where
  insert mateId mate canBeOverwritten = do
    Env {mates} <- ask
    liftIO . atomically . stateTVar mates $ \m ->
      case m Map.!? mateId of
        Nothing -> (Nothing, Map.insert mateId mate m)
        Just n | canBeOverwritten n -> (Nothing, Map.insert mateId mate m)
        Just n | otherwise -> (Just AlreadyPresent, m)

  remove mateId canBeRemoved = do
    Env {mates} <- ask
    liftIO . atomically . stateTVar mates $ \m ->
      case m Map.!? mateId of
        Nothing -> (Just NotFound, m)
        Just n | canBeRemoved n -> (Just Forbidden, m)
        Just n | otherwise -> (Nothing, Map.delete mateId m)
