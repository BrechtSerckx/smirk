module Smirk.Env
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
  { nodes :: TVar (Map NodeId Node)
  }

mkEnv :: IO Env
mkEnv = do
  nodes <- newTVarIO Map.empty
  pure Env {..}
