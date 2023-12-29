module Smirk.Captain.Api
  ( Api,
    Routes (..),
  )
where

import GHC.Generics (Generic)
import Servant.API
import qualified Smirk.Captain.Register.Api as Register
import Smirk.Types (MateId)

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] (),
    registerApi :: mode :- "register" :> Register.Api,
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
