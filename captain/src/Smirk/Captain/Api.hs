module Smirk.Captain.Api
  ( Api,
    Routes (..),
  )
where

import GHC.Generics (Generic)
import Servant.API
import qualified Smirk.Captain.Pair.Api as Pair
import Smirk.Types (MateId)

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] (),
    pairApi :: mode :- "pair" :> Pair.Api,
    send ::
      mode
        :- "mate"
          :> Capture "id" MateId
          :> "send"
          :> ReqBody '[JSON] ()
          :> Post '[JSON] ()
  }
  deriving (Generic)

type Api = ToServantApi Routes
