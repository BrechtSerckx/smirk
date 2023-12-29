module Smirk (runSmirk) where

import Control.Monad.Logger (runStderrLoggingT)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Smirk.Env (mkEnv)
import Smirk.Prelude
import Smirk.Server (app)

runSmirk :: IO ()
runSmirk = do
  env <- mkEnv
  runStderrLoggingT $ do
    $logInfo "Starting server"
    liftIO
      . Warp.run 8080
      . (if True then Wai.logStdoutDev else Wai.logStdout)
      $ app env
