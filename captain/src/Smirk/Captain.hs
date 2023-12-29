module Smirk.Captain (runSmirkCaptain) where

import Control.Monad.Logger (runStderrLoggingT)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Smirk.Captain.Env (mkEnv)
import Smirk.Captain.Server (app)
import Smirk.Prelude

runSmirkCaptain :: IO ()
runSmirkCaptain = do
  env <- mkEnv
  runStderrLoggingT $ do
    $logInfo "Starting SMIRK captain"
    liftIO
      . Warp.run 8080
      . (if True then Wai.logStdoutDev else Wai.logStdout)
      $ app env
