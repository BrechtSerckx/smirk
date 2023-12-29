module Smirk.Mate.Api
  ( Api,
    Routes (..),
  )
where

import GHC.Generics (Generic)
import Servant.API
import Smirk.Prelude

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] (),
    send ::
      mode
        :- "send"
          :> Header' '[Strict] "Authorization" Text
          :> ReqBody '[JSON] ()
          :> Post '[JSON] ()
  }
  deriving (Generic)

type Api = ToServantApi Routes
