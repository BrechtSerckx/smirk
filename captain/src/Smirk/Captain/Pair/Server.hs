module Smirk.Captain.Pair.Server (server) where

import Servant.Server (ServerError (..), err403, err404, err409)
import Servant.Server.Generic (AsServerT)
import Smirk.Captain.MateStore (MonadMateStore)
import qualified Smirk.Captain.MateStore as MateStore
import Smirk.Captain.Pair.Api
  ( PairData (..),
    Routes (..),
    UnpairData (..),
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
    { pair =
        \PairData {accessToken = mAccessToken, ..} -> do
          $logInfo [qq|Pairing mate: $mateId|]
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
                      [qq|There is already a mate paired for id $mateId|]
                  }
            Nothing -> pure ()
          $logInfo [qq|Registered mate: $mateId|]
          return mate,
      unpair =
        \UnpairData {..} -> do
          $logInfo [qq|Unpairing mate: $mateId|]
          mErr <-
            MateStore.remove
              mateId
              (\Mate {accessToken = accessToken'} -> accessToken' == accessToken)
          case mErr of
            Just MateStore.NotFound -> throwM err404
            Just MateStore.Forbidden -> throwM err403
            Nothing -> pure ()
          $logInfo [qq|Unpairing mate: $mateId|]
    }
