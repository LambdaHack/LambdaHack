{-# LANGUAGE DeriveGeneric #-}
-- | Terrain tile features.
module Game.LambdaHack.Common.Feature
  ( Feature(..)
  ) where

import Data.Binary
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Misc

-- | All possible terrain tile features.
data Feature =
    Cause !(Effect.Effect Int)  -- ^ causes the effect when triggered
  | OpenTo !GroupName    -- ^ goes from a closed to an open tile when altered
  | CloseTo !GroupName   -- ^ goes from an open to a closed tile when altered
  | ChangeTo !GroupName  -- ^ alters tile, but does not change walkability
  | HideAs !GroupName    -- ^ when hidden, looks as a tile of the group
  | RevealAs !GroupName  -- ^ if secret, can be revealed to belong to the group

  | Walkable             -- ^ actors can walk through
  | Clear                -- ^ actors can see through
  | Dark                 -- ^ is not lit with an ambient shine
  | Suspect              -- ^ may not be what it seems (clients only)
  | Aura !(Effect.Effect Int)  -- ^ sustains the effect continuously, TODO
  | Impenetrable         -- ^ can never be excavated nor seen through

  | OftenItem            -- ^ initial items often generated there
  | OftenActor           -- ^ initial actors and stairs often generated there
  | NoItem               -- ^ no items ever generated there
  | NoActor              -- ^ no actors nor stairs ever generated there
  | Trail                -- ^ used for visible trails throughout the level
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary Feature

instance Hashable Feature
