module Smirk.Register.Api
  ( Routes (..),
    Api,
    RegisterData (..),
    DeregisterData (..),
  )
where

import Servant.API
import Smirk.Prelude
import Smirk.Types (AccessToken, Node, NodeId)

data RegisterData = RegisterData
  {nodeId :: NodeId}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data DeregisterData = DeregisterData
  {nodeId :: NodeId, accessToken :: AccessToken}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Routes api = Routes
  { register ::
      api
        :- ReqBody '[JSON] RegisterData
          :> Post '[JSON] Node,
    deregister ::
      api
        :- ReqBody '[JSON] DeregisterData
          :> Post '[JSON] ()
  }
  deriving (Generic)

type Api = ToServantApi Routes
