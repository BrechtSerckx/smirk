module Smirk (runSmirk) where

import qualified Network.Wai.Handler.Warp as Warp
import Smirk.Env (mkEnv)
import Smirk.Prelude
import Smirk.Server (app)

runSmirk :: IO ()
runSmirk = do
  env <- mkEnv
  Warp.run 8080 (app env)
