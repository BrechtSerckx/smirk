module Smirk.Captain.Register.Api
  ( Routes (..),
    Api,
    RegisterData (..),
    DeregisterData (..),
  )
where

import Servant.API
import qualified Servant.Client as Servant (BaseUrl)
import Smirk.Prelude
import Smirk.Types (AccessToken, Mate, MateId)

data RegisterData = RegisterData
  { mateId :: MateId,
    baseUrl :: Servant.BaseUrl,
    accessToken :: Maybe AccessToken
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data DeregisterData = DeregisterData
  {mateId :: MateId, accessToken :: AccessToken}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Routes api = Routes
  { register ::
      api
        :- ReqBody '[JSON] RegisterData
          :> Post '[JSON] Mate,
    deregister ::
      api
        :- ReqBody '[JSON] DeregisterData
          :> Post '[JSON] ()
  }
  deriving (Generic)

type Api = ToServantApi Routes
