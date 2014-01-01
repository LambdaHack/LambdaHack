{-# LANGUAGE DeriveGeneric #-}
-- | Item features.
module Game.LambdaHack.Common.ItemFeature
  ( Feature(..)
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Random

-- | All possible item features.
data Feature =
    Cause !(Effect RollDeep)  -- ^ causes the effect when triggered
  | ChangeTo !Text            -- ^ changes to this item kind group when altered
  | Explode !Text             -- ^ explode, producing this group of shrapnel
  | Fragile                   -- ^ breaks even when not hitting an enemy
  | Linger !Int               -- ^ fly for this percentage of 2 turns
  deriving (Show, Eq, Ord, Generic)

instance Binary Feature
