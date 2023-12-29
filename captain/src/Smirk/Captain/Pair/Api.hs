module Smirk.Captain.Pair.Api
  ( Routes (..),
    Api,
    PairData (..),
    UnpairData (..),
  )
where

import Servant.API
import qualified Servant.Client as Servant (BaseUrl)
import Smirk.Prelude
import Smirk.Types (AccessToken, Mate, MateId)

data PairData = PairData
  { mateId :: MateId,
    baseUrl :: Servant.BaseUrl,
    accessToken :: Maybe AccessToken
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data UnpairData = UnpairData
  {mateId :: MateId, accessToken :: AccessToken}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Routes api = Routes
  { pair ::
      api
        :- ReqBody '[JSON] PairData
          :> Post '[JSON] Mate,
    unpair ::
      api
        :- ReqBody '[JSON] UnpairData
          :> Post '[JSON] ()
  }
  deriving (Generic)

type Api = ToServantApi Routes
