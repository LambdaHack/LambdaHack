module Game.LambdaHack.Feature
  ( Feature(..)
  ) where

import Game.LambdaHack.Effect
import Game.LambdaHack.Random

data Feature =
    Walkable           -- ^ actors can walk through
  | Clear              -- ^ actors can see through
  | Exit               -- ^ is an exit from a room
  | Lit                -- ^ is lit; TODO: (partially) replace ucolor by this feature?
  | Secret !RollDice   -- ^ tile is generated with this high secrecy value
  | Aura !Effect       -- ^ sustains the effect continuously
  | Cause !Effect      -- ^ causes the effect when triggered
  | Change !Char       -- ^ transitions when triggered
  | Climbable          -- ^ triggered by climbing
  | Descendable        -- ^ triggered by descending into
  | Openable           -- ^ triggered by opening
  | Closable           -- ^ triggered by closable
  | Hidden             -- ^ triggered when the tile's secrecy becomes (Just 0)
  | Special            -- ^ a variation for special flavour
  | Boring             -- ^ items and stairs can be generated there
  deriving (Show, Eq, Ord)
