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
import Smirk.Prelude
import Smirk.Types (Node, NodeId)

data Env = Env
  { mates :: TVar (Map NodeId Node)
  }

mkEnv :: IO Env
mkEnv = do
  mates <- newTVarIO Map.empty
  pure Env {..}
