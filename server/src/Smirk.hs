module Smirk (runSmirk) where

import Smirk.Env (Env (..))
import Smirk.Prelude

runSmirk :: IO ()
runSmirk = putStrLn "someFunc"
