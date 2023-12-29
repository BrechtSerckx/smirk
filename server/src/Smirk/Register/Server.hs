module Smirk.Register.Server (server) where

import Servant.Server (ServerError (..), err403, err404, err409)
import Servant.Server.Generic (AsServerT)
import Smirk.Prelude
import Smirk.Register.Api
  ( DeregisterData (..),
    RegisterData (..),
    Routes (..),
  )
import Smirk.Register.Class
  ( DeregisterError (..),
    MonadRegister,
    RegisterError (..),
    deregisterNode,
    registerNode,
  )
import Smirk.Types (Node (..), genAccessToken)

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
          $logInfo [qq|Registering node: $nodeId|]
          accessToken <- genAccessToken
          let node = Node {..}
          mErr <- registerNode nodeId node
          case mErr of
            Just AlreadyRegistered ->
              throwM
                err409
                  { errBody = "There is already a node registered with id ???"
                  }
            Nothing -> pure ()
          $logInfo [qq|Registered node: $nodeId|]
          return node,
      deregister =
        \DeregisterData {..} -> do
          $logInfo [qq|Deregistering node: $nodeId|]
          mErr <- deregisterNode nodeId accessToken
          case mErr of
            Just NotFound -> throwM err404
            Just Unauthorized -> throwM err403
            Nothing -> pure ()
          $logInfo [qq|Deregistered node: $nodeId|]
    }
