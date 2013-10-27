{-# LANGUAGE DeriveGeneric #-}
-- | Terrain tile features.
module Game.LambdaHack.Common.Feature
  ( Feature(..)
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Effect

-- | All possible terrain tile features, some of them parameterized
-- or dependent on outside coefficients, e.g., on the tile secrecy value.
data Feature =
    Ascendable           -- ^ triggered by ascending
  | Descendable          -- ^ triggered by descending
  | Escapable            -- ^ triggered by escaping
  | Openable             -- ^ triggered by opening
  | Closable             -- ^ triggered by closing

  | Cause !(Effect Int)  -- ^ causes the effect when triggered
  | ChangeTo !Text       -- ^ transitions to a tile of the group when triggered

  | Walkable             -- ^ actors can walk through
  | Clear                -- ^ actors can see through
  | Lit                  -- ^ is lit with an ambient shine
  | Suspect              -- ^ may not be what it seems (clients only)
  | Aura !(Effect Int)   -- ^ sustains the effect continuously, TODO
  | Impenetrable         -- ^ can never be excavated nor seen through

  | CanItem              -- ^ items can be generated there
  | CanActor             -- ^ actors and stairs can be generated there
  | Exit                 -- ^ is a (not hidden) door, stair, etc.
  | Path                 -- ^ used for visible paths throughout the level
  | HiddenAs !Text       -- ^ when hidden, looks as a tile of the group
  | RevealAs !Text       -- ^ if secret, can be revealed to belong to the group
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary Feature
