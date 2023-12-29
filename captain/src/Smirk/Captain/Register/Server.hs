module Smirk.Captain.Register.Server (server) where

import Servant.Server (ServerError (..), err403, err404, err409)
import Servant.Server.Generic (AsServerT)
import Smirk.Captain.MateStore (MonadMateStore)
import qualified Smirk.Captain.MateStore as MateStore
import Smirk.Captain.Register.Api
  ( DeregisterData (..),
    RegisterData (..),
    Routes (..),
  )
import Smirk.Prelude
import Smirk.Types (Mate (..), genAccessToken)

server ::
  forall m.
  ( MonadRandom m,
    MonadMateStore m,
    MonadThrow m,
    MonadLogger m
  ) =>
  Routes (AsServerT m)
server =
  Routes
    { register =
        \RegisterData {accessToken = mAccessToken, ..} -> do
          $logInfo [qq|Registering mate: $mateId|]
          accessToken <- maybe genAccessToken pure mAccessToken
          let mate = Mate {..}
          mErr <-
            MateStore.insert
              mateId
              mate
              ( \Mate {accessToken = accessToken'} ->
                  Just accessToken' == mAccessToken
              )
          case mErr of
            Just MateStore.AlreadyPresent ->
              throwM
                err409
                  { errBody =
                      [qq|There is already a mate registered with id $mateId|]
                  }
            Nothing -> pure ()
          $logInfo [qq|Registered mate: $mateId|]
          return mate,
      deregister =
        \DeregisterData {..} -> do
          $logInfo [qq|Deregistering mate: $mateId|]
          mErr <-
            MateStore.remove
              mateId
              (\Mate {accessToken = accessToken'} -> accessToken' == accessToken)
          case mErr of
            Just MateStore.NotFound -> throwM err404
            Just MateStore.Forbidden -> throwM err403
            Nothing -> pure ()
          $logInfo [qq|Deregistered mate: $mateId|]
    }
