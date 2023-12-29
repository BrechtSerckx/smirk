module Smirk.Types
  ( NodeId,
    Node (..),
    AccessToken,
    genAccessToken,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Smirk.Prelude

type NodeId = Text

type AccessToken = Text

accessTokenLength :: Int
accessTokenLength = 24

genAccessToken :: MonadRandom m => m AccessToken
genAccessToken =
  Text.take accessTokenLength
    . Text.filter Char.isLetter
    . Text.pack
    <$> getRandomRs ('A', 'z')

data Node = Node {accessToken :: AccessToken}
  deriving stock (Generic)
  deriving anyclass (ToJSON)
