{-# LANGUAGE DeriveGeneric #-}
-- | Item features.
module Game.LambdaHack.Common.ItemFeature
  ( Feature(..)
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect

-- | All possible item features.
data Feature =
    Cause !(Effect Dice.Dice)  -- ^ causes the effect when triggered
  | ChangeTo !Text             -- ^ changes to this item kind group when altered
  | Explode !Text              -- ^ explode, producing this group of shrapnel
  | Fragile                    -- ^ breaks even when not hitting an enemy
  | Linger !Int                -- ^ fly for this percentage of 2 turns
  | Consumable                 -- ^ can't be turned off, is consumed by use
  deriving (Show, Eq, Ord, Generic)

instance Binary Feature
