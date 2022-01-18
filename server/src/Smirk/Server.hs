{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Smirk.Server
  ( runSmirkServer
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Acquire                   ( withAcquire )
import           Data.Aeson
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant.API
import qualified Servant.Server                as Servant

import           Smirk.M

data IrSignalType = Raw
instance FromJSON IrSignalType where
  parseJSON = withText "IrSignalType" $ \case
    "raw" -> pure Raw
    t     -> fail $ "Unknown IrSignalType: " <> Text.unpack t
instance ToJSON IrSignalType where
  toJSON = \case
    Raw -> String "raw"
data IrSignal = IrSignal
  { format :: IrSignalType
  , freq   :: Int
  , data_  :: [Int]
  }
instance FromJSON IrSignal where
  parseJSON = withObject "IrSignal" $ \o -> do
    format <- o .: "format"
    freq   <- o .: "freq"
    data_  <- o .: "data"
    pure IrSignal { .. }
instance ToJSON IrSignal where
  toJSON IrSignal {..} =
    object ["format" .= format, "freq" .= freq, "data" .= data_]

-- brittany-disable-next-binding
type SmirkApi
  =  "messages"
  :>  (  Get '[JSON] IrSignal
    :<|> ReqBody '[JSON] IrSignal
      :> Post '[JSON] ()
      )

pSmirkApi :: Proxy SmirkApi
pSmirkApi = Proxy

smirkServer :: Monad m => Servant.ServerT SmirkApi m
smirkServer = getLatestIrSignal :<|> sendIrSignal
 where
  getLatestIrSignal = pure IrSignal { format = Raw, freq = 38, data_ = [] }
  sendIrSignal _ = pure ()

mToHandler :: MkCtx -> forall a . M a -> Servant.Handler a
mToHandler (acquireSerialPort, mkCtx) act =
  liftIO . withAcquire acquireSerialPort $ \serialPort ->
    act `runM` mkCtx serialPort

runSmirkServer :: Warp.Settings -> MkCtx -> IO ()
runSmirkServer warpSettings mkCtx =
  Warp.runSettings warpSettings
    . Servant.serve pSmirkApi
    . Servant.hoistServer pSmirkApi (mToHandler mkCtx)
    $ smirkServer
