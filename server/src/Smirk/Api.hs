module Smirk.Api
  ( Api,
    Routes (..),
  )
where

import GHC.Generics (Generic)
import Servant.API
import qualified Smirk.Register.Api

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] (),
    registerApi :: mode :- "register" :> Smirk.Register.Api.Api
  }
  deriving (Generic)

type Api = ToServantApi Routes
