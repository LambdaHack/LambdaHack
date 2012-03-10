-- | Terrain tile features.
module Game.LambdaHack.Feature
  ( Feature(..)
  ) where

import Game.LambdaHack.Effect
import Game.LambdaHack.Random

-- | All possible terrain tile features, some of them parameterized or dependent
-- on outside coefficients, e.g., on the tile secrecy value.
data Feature =
    Ascendable         -- ^ triggered by ascending
  | Descendable        -- ^ triggered by descending
  | Openable           -- ^ triggered by opening
  | Closable           -- ^ triggered by closing
  | Hidden             -- ^ triggered when the tile's secrecy becomes zero

  | Cause !Effect      -- ^ causes the effect when triggered
  | ChangeTo !String   -- ^ transitions to any tile of the group when triggered

  | Walkable           -- ^ actors can walk through
  | Clear              -- ^ actors can see through
  | Lit                -- ^ is lit with an ambient shine
  | Aura !Effect       -- ^ sustains the effect continuously, TODO

  | Boring             -- ^ items and stairs can be generated there
  | Exit               -- ^ is a (not hidden) door, stair, etc.
  | Path               -- ^ used for distinct paths throughout the level
  | Secret !RollDice   -- ^ discovering the secret will require this many steps
  deriving (Show, Read, Eq, Ord)
