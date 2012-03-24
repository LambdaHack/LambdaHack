-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Point
  ( Point, toPoint, showPoint
  , origin, chessDist, adjacent, vicinity, vicinityCardinal
  , inside, displacementXYZ, bla
  ) where

import qualified Data.List as L

import Game.LambdaHack.PointXY
import Game.LambdaHack.VectorXY
import Game.LambdaHack.Area
import Game.LambdaHack.Utils.Assert

-- | The type of locations on the 2D level map, heavily optimized.
--
-- We represent the (level map on the) screen as a linear framebuffer,
-- where @Point@ is an @Int@ offset counted from the first cell.
-- We do bounds check for the X size whenever we convert between
-- representations and each subsequent
-- array access performs another check, effectively for Y size.
-- After dungeon is generated (using @PointXY@, not @Point@),
-- and converted to the @Point@ representation, points are used
-- mainly as keys and not constructed often, so the performance will improve
-- due to smaller save files, the use of @IntMap@ and cheaper array indexing,
-- including cheaper bounds checks.
-- We don't defin @Point@ as a newtype to avoid the trouble
-- with using @EnumMap@ in place of @IntMap@, etc.
type Point = Int

-- | Print a point as a tuple of cartesian coordinates.
showPoint :: X -> Point -> String
showPoint lxsize = show . fromPoint lxsize

-- | Conversion from cartesian coordinates to @Point@.
toPoint :: X -> PointXY -> Point
toPoint lxsize (PointXY (x, y)) =
  assert (lxsize > x && x >= 0 && y >= 0 `blame` (lxsize, x, y)) $
  x + y * lxsize

-- | Conversion from @Point@ to cartesian coordinates.
fromPoint :: X -> Point -> PointXY
fromPoint lxsize loc =
  assert (loc >= 0 `blame` (lxsize, loc)) $
  PointXY (loc `rem` lxsize, loc `quot` lxsize)

-- | The top-left corner location of the level.
origin :: Point
origin = 0

-- | The distance between two points in the chessboard metric.
chessDist :: X -> Point -> Point -> Int
chessDist lxsize loc0 loc1
  | PointXY (x0, y0) <- fromPoint lxsize loc0
  , PointXY (x1, y1) <- fromPoint lxsize loc1 =
  chessDistXY $ VectorXY (x1 - x0, y1 - y0)

-- | Checks whether two points are adjacent on the map
-- (horizontally, vertically or diagonally).
adjacent :: X -> Point -> Point -> Bool
adjacent lxsize s t = chessDist lxsize s t == 1

-- | Returns the 8, or less, surrounding locations of a given location.
vicinity :: X -> Y -> Point -> [Point]
vicinity lxsize lysize loc =
  map (toPoint lxsize) $
    vicinityXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint lxsize loc

-- | Returns the 4, or less, surrounding locations in cardinal directions
-- from a given location.
vicinityCardinal :: X -> Y -> Point -> [Point]
vicinityCardinal lxsize lysize loc =
  map (toPoint lxsize) $
    vicinityCardinalXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint lxsize loc

-- | Checks that a point belongs to an area.
inside :: X -> Point -> Area -> Bool
inside lxsize loc = insideXY $ fromPoint lxsize loc

-- | Calculate the displacement vector from a location to another.
displacementXYZ :: X -> Point -> Point -> VectorXY
displacementXYZ lxsize loc0 loc1
  | PointXY (x0, y0) <- fromPoint lxsize loc0
  , PointXY (x1, y1) <- fromPoint lxsize loc1 =
  VectorXY (x1 - x0, y1 - y0)

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA).
-- Skips the source point and goes through the second point
-- to the edge of the level. GIves @Nothing@ if the points are equal.
bla :: X -> Y -> Int -> Point -> Point -> Maybe [Point]
bla _ _ _ source target | source == target = Nothing
bla lxsize lysize eps source target = Just $
  let s = fromPoint lxsize source
      e = fromPoint lxsize target
      inBounds p@(PointXY (x, y)) =
        lxsize > x && x >= 0 && lysize > y && y >= 0 && p /= s
  in L.map (toPoint lxsize) $ L.takeWhile inBounds $ L.tail $ blaXY eps s e
