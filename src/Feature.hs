module Feature (Feature(..)) where

import Effect
import Random

data Feature =
    Walkable           -- ^ actors can walk through
  | Clear              -- ^ actors can see through
  | Exit               -- ^ is an exit from a room
  | Lit                -- ^ is lit; TODO: (partially) replace ucolor by this feature?
  | Secret !RollDice   -- ^ tile is generated with this high tsecret field
  | Aura !Effect       -- ^ sustains the effect continuously
  | Cause !Effect      -- ^ causes the effect when triggered
  | Change !Char       -- ^ transitions when triggered
  | Climbable          -- ^ triggered by climbing
  | Descendable        -- ^ triggered by descending into
  | Openable           -- ^ triggered by opening
  | Closable           -- ^ triggered by closable
  | Hidden             -- ^ triggered when the tile's tsecret becomes (Just 0)
  deriving (Show, Eq, Ord)
