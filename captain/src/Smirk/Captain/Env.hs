module Smirk.Captain.Env
  ( Env (..),
    mkEnv,
  )
where

import Control.Concurrent.STM.TVar
  ( TVar,
    newTVarIO,
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HTTP.Client
import qualified Servant.Client as Servant
  ( ClientEnv,
    mkClientEnv,
  )
import Smirk.Prelude
import Smirk.Types (Mate, MateId)

data Env = Env
  { mates :: TVar (Map MateId Mate),
    servantClientEnv :: Servant.ClientEnv
  }

mkEnv :: IO Env
mkEnv = do
  mates <- newTVarIO Map.empty
  httpManager <- HTTP.Client.newManager HTTP.Client.defaultManagerSettings
  let servantClientEnv =
        Servant.mkClientEnv
          httpManager
          (error "Please override the base-url on each call!")
  pure Env {..}
