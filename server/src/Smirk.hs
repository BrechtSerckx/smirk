{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Smirk
  ( main
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

import           Smirk.Control
import           Smirk.M
import           Smirk.Opts
import           Smirk.Server                   ( runSmirkServer )

main :: IO ()
main = do
  opts@Opts {..} <- parseOpts
  case cmd of
    Control controlCmd -> do
      runWithCtx :: RunWithCtx <- mkRunWithCtx opts
      runWithCtx . runM $ do
        res <- case controlCmd of
          Ping    -> ping >> pure "pong"
          Version -> version
          Send s  -> send s >> pure "OK"
          Receive -> Text.pack . show <$> receive
        liftIO $ Text.putStrLn res
    Serve warpSettings -> do
      runWithCtx :: RunWithCtx <- mkRunWithCtx opts
      runSmirkServer warpSettings runWithCtx
