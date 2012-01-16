-- | Terrain tile features.
module Game.LambdaHack.Feature
  ( Feature(..)
  ) where

import Game.LambdaHack.Effect
import Game.LambdaHack.Random

-- | All possible terrain tile features, some of them parameterized or dependent
-- on outside coefficients, e.g., on the tile secrecy value.
data Feature =
    Walkable           -- ^ actors can walk through
  | Clear              -- ^ actors can see through
  | Exit               -- ^ is a not hidden door, stair, etc.
  | Lit                -- ^ is lit with an ambient shine
  | Secret !RollDice   -- ^ tile is generated with this high secrecy value
  | Aura !Effect       -- ^ sustains the effect continuously, TODO
  | Cause !Effect      -- ^ causes the effect when triggered
  | ChangeTo !String   -- ^ transitions to any tile of this group when triggered
  | Ascendable         -- ^ triggered by ascending
  | Descendable        -- ^ triggered by descending
  | Openable           -- ^ triggered by opening
  | Closable           -- ^ triggered by closing
  | Hidden             -- ^ triggered when the tile's secrecy becomes zero
  | Path               -- ^ used for distinct paths throughout the level
  | Boring             -- ^ items and stairs can be generated there
  deriving (Show, Read, Eq, Ord)
