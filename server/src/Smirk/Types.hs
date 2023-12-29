module Smirk.Types
  ( NodeId,
    Node (..),
    AccessToken,
    genAccessToken,
  )
where

import qualified Data.Char as Char
import Smirk.Prelude

type NodeId = String

type AccessToken = String

accessTokenLength :: Int
accessTokenLength = 24

genAccessToken :: MonadRandom m => m AccessToken
genAccessToken = take accessTokenLength . filter Char.isLetter <$> getRandomRs ('A', 'z')

data Node = Node {accessToken :: AccessToken}
  deriving stock (Generic)
  deriving anyclass (ToJSON)
