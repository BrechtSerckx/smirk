{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Capability.ReaderTVar where

import           Capability.Reader              ( HasReader(..)
                                                , ask
                                                )
import           Capability.Sink                ( HasSink(..) )
import           Capability.Source              ( HasSource(..)
                                                , await
                                                )
import           Capability.State               ( HasState(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )

import           Control.Concurrent.STM

newtype ReaderTVar m a = ReaderTVar (m a)
  deriving newtype (Functor, Applicative, Monad)

instance
  (HasSource tag (TVar s) m, MonadIO m)
  => HasSource tag s (ReaderTVar m)
  where
  await_ _ = ReaderTVar $ do
    ref <- await @tag
    liftIO $ readTVarIO ref
  {-# INLINE await_ #-}

instance
  (HasSource tag (TVar s) m, MonadIO m)
  => HasSink tag s (ReaderTVar m)
  where
  yield_ _ v = ReaderTVar $ do
    ref <- await @tag
    liftIO . atomically $ writeTVar ref v
  {-# INLINE yield_ #-}

instance
  (HasReader tag (TVar s) m, MonadIO m)
  => HasState tag s (ReaderTVar m)
  where
  state_ _ f = ReaderTVar $ do
    ref <- ask @tag
    liftIO . atomically $ stateTVar ref f
  {-# INLINE state_ #-}
