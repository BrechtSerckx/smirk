module Smirk.Register.Server (server) where

import Servant.Server
import Servant.Server.Generic (AsServerT)
import Smirk.Prelude
import Smirk.Register.Api
  ( RegisterData (..),
    Routes (..),
  )
import Smirk.Register.Class
  ( MonadRegister,
    RegisterError (..),
    registerNode,
  )
import Smirk.Types (Node (..), genAccessToken)

server ::
  forall m.
  ( MonadRandom m,
    MonadRegister m,
    MonadThrow m
  ) =>
  Routes (AsServerT m)
server =
  Routes
    { register =
        \RegisterData {..} -> do
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
          return node
    }
