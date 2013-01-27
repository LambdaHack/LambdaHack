{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Abstract faction identifiers. (In a separate module to break a dependency
-- cycle.)
module Game.LambdaHack.FactionId (FactionId, invalidFactionId) where

import Data.Binary

-- | A unique identifier of a faction in a game.
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum)

instance Binary FactionId where
  put (FactionId n) = put n
  get = fmap FactionId get

invalidFactionId :: FactionId
invalidFactionId = FactionId (-1)
