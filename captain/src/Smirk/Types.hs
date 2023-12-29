module Smirk.Types
  ( MateId,
    Mate (..),
    AccessToken,
    genAccessToken,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Servant.Client as Servant
import Smirk.Prelude

type MateId = Text

type AccessToken = Text

accessTokenLength :: Int
accessTokenLength = 24

genAccessToken :: MonadRandom m => m AccessToken
genAccessToken =
  Text.take accessTokenLength
    . Text.filter Char.isLetter
    . Text.pack
    <$> getRandomRs ('A', 'z')

data Mate = Mate {accessToken :: AccessToken, baseUrl :: Servant.BaseUrl}
  deriving stock (Generic)
  deriving anyclass (ToJSON)
