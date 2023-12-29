module Smirk.Captain.Register.Server (server) where

import Servant.Server (ServerError (..), err403, err404, err409)
import Servant.Server.Generic (AsServerT)
import Smirk.Captain.Register.Api
  ( DeregisterData (..),
    RegisterData (..),
    Routes (..),
  )
import Smirk.Captain.Register.Class
  ( DeregisterError (..),
    MonadRegister,
    RegisterError (..),
    deregisterMate,
    registerMate,
  )
import Smirk.Prelude
import Smirk.Types (Mate (..), genAccessToken)

server ::
  forall m.
  ( MonadRandom m,
    MonadRegister m,
    MonadThrow m,
    MonadLogger m
  ) =>
  Routes (AsServerT m)
server =
  Routes
    { register =
        \RegisterData {..} -> do
          $logInfo [qq|Registering mate: $mateId|]
          accessToken <- genAccessToken
          let mate = Mate {..}
          mErr <- registerMate mateId mate
          case mErr of
            Just AlreadyRegistered ->
              throwM
                err409
                  { errBody = "There is already a mate registered with id ???"
                  }
            Nothing -> pure ()
          $logInfo [qq|Registered mate: $mateId|]
          return mate,
      deregister =
        \DeregisterData {..} -> do
          $logInfo [qq|Deregistering mate: $mateId|]
          mErr <- deregisterMate mateId accessToken
          case mErr of
            Just NotFound -> throwM err404
            Just Unauthorized -> throwM err403
            Nothing -> pure ()
          $logInfo [qq|Deregistered mate: $mateId|]
    }
