module Smirk (runSmirk) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Smirk.Env (mkEnv)
import Smirk.Prelude
import Smirk.Server (app)

runSmirk :: IO ()
runSmirk = do
  env <- mkEnv
  putStrLn "Starting server"
  Warp.run 8080 . (if True then Wai.logStdoutDev else Wai.logStdout) $
    app env
