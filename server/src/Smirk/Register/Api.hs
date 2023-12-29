module Smirk.Register.Api (Routes (..), Api, RegisterData (..)) where

import Servant.API
import Smirk.Prelude
import Smirk.Types (Node, NodeId)

data RegisterData = RegisterData
  {nodeId :: NodeId}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Routes api = Routes
  { register ::
      api
        :- ReqBody '[JSON] RegisterData
          :> Post '[JSON] Node
  }
  deriving (Generic)

type Api = ToServantApi Routes
