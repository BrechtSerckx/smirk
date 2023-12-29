module Smirk.Mate.Client () where

import Servant.Client.Core (RunClient)
import Servant.Client.Generic
  ( AsClientT,
    genericClient,
  )
import Smirk.Mate.Api (Api, Routes (..))
import Smirk.Prelude

client :: RunClient m => Routes (AsClientT m)
client = genericClient

getVersion :: RunClient m => m ()
getVersion = version client

doSend :: RunClient m => Maybe Text -> () -> m ()
doSend = send client
