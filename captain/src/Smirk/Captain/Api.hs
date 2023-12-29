module Smirk.Captain.Api
  ( Api,
    Routes (..),
  )
where

import GHC.Generics (Generic)
import Servant.API
import qualified Smirk.Captain.Register.Api as Register

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] (),
    registerApi :: mode :- "register" :> Register.Api
  }
  deriving (Generic)

type Api = ToServantApi Routes
