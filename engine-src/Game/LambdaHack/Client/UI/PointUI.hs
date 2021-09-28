-- | UI screen coordinates.
module Game.LambdaHack.Client.UI.PointUI
  ( PointUI(..), PointSquare(..), squareToUI, uiToSquare
  , squareToMap, mapToSquare
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , mapStartY
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Common.Point

-- | UI screen coordinates, independent of whether square or monospace fonts
-- are being placed on the screen (though square fonts are never placed
-- on odd coordinates). These are not game map coordinates,
-- becuse UI is larger and more fine-grained than just the game map.
data PointUI = PointUI Int Int
  deriving (Show, Eq)

-- | Coordinates of the big square fonts. These are not game map coordinates,
-- because the latter are offset by @mapStartY@ and represented by @Point@.
data PointSquare = PointSquare Int Int
  deriving (Show, Eq)

squareToUI :: PointSquare -> PointUI
{-# INLINE squareToUI #-}
squareToUI (PointSquare x y) = PointUI (x * 2) y

uiToSquare :: PointUI -> PointSquare
{-# INLINE uiToSquare #-}
uiToSquare (PointUI x y) = PointSquare (x `div` 2) y

-- | The row where the dungeon map starts, both in @PointUI@
-- and @PointSquare@ coordinates.
mapStartY :: Int
mapStartY = 1

squareToMap :: PointSquare -> Point
{-# INLINE squareToMap #-}
squareToMap (PointSquare x y) = Point x (y - mapStartY)

mapToSquare :: Point -> PointSquare
{-# INLINE mapToSquare #-}
mapToSquare (Point x y) = PointSquare x (y + mapStartY)
