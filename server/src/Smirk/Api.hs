module Smirk.Api (Api, Routes (..)) where

import GHC.Generics (Generic)
import Servant.API

data Routes mode = Routes
  { version :: mode :- "version" :> Get '[JSON] ()
  }
  deriving (Generic)

type Api = ToServantApi Routes
