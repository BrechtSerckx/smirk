module Smirk.Env (Env (..), mkEnv) where

import Smirk.Prelude

data Env = Env

mkEnv :: IO Env
mkEnv = do
  pure Env {}
