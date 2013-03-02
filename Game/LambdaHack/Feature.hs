-- | Terrain tile features.
module Game.LambdaHack.Feature
  ( Feature(..)
  ) where

import Data.Text (Text)

import Game.LambdaHack.Effect
import Game.LambdaHack.Random

-- | All possible terrain tile features, some of them parameterized
-- or dependent on outside coefficients, e.g., on the tile secrecy value.
data Feature =
    Ascendable         -- ^ triggered by ascending
  | Descendable        -- ^ triggered by descending
  | Openable           -- ^ triggered by opening
  | Closable           -- ^ triggered by closing
  | Hidden             -- ^ triggered when the tile's secrecy becomes zero

  | Cause !(Effect Int)  -- ^ causes the effect when triggered
  | ChangeTo !Text     -- ^ transitions to any tile of the group when triggered

  | Walkable           -- ^ actors can walk through
  | Clear              -- ^ actors can see through
  | Lit                -- ^ is lit with an ambient shine
  | Aura !(Effect Int)  -- ^ sustains the effect continuously, TODO

  | Boring             -- ^ items and stairs can be generated there
  | Exit               -- ^ is a (not hidden) door, stair, etc.
  | Path               -- ^ used for visible paths throughout the level
  | Secret !RollDice   -- ^ discovering the secret will require this many turns
  | Impenetrable       -- ^ can never be excavated nor seen through
  deriving (Show, Read, Eq, Ord)
