-- | UI screen coordinates.
module Game.LambdaHack.Client.UI.PointUI
  ( PointUI(..), mapStartY
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

-- | Screen coordinates, independent of whether square or monospace fonts
-- are being placed on the screen. These are not game map coordinates,
-- becuse UI is larger than just the game map.
data PointUI = PointUI Int Int
  deriving (Eq, Show)

-- | The row where the dungeon map starts.
mapStartY :: Int
mapStartY = 1
