-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Point
  ( Point, toPoint, fromPoint, showPoint
  , origin, chessDist, adjacent, vicinity
  ) where

import Game.LambdaHack.PointXY
import Game.LambdaHack.VectorXY
import Game.LambdaHack.Area
import Game.LambdaHack.Utils.Assert

-- TODO: limit the places toPoint and fromPoint are used.
-- | The type of locations on the 2D level map, heavily optimized.
--
-- We represent the (level map on the) screen as a linear framebuffer,
-- where @Point@ is an @Int@ offset counted from the first cell.
-- We do bounds check for the X size ASAP and each subsequent
-- array access performs another check, effectively for Y size.
-- After dungeon is generated (using @PointXY@, not @Point@),
-- and converted to the @Point@ representation, points are used
-- mainly as keys and not constructed often, so the performance will improve
-- due to smaller save files, the use of @IntMap@ and cheaper array indexing,
-- including cheaper bounds checks.
-- We don't use a newtype to avoid the trouble with using @EnumMap@
-- in place of @IntMap@, etc.
type Point = Int

-- | Print a point as a tuple of cartesian coordinates.
showPoint :: X -> Point -> String
showPoint lxsize = show . fromPoint lxsize

-- | Conversion from cartesian coordinates to @Point@.
toPoint :: X -> PointXY -> Point
toPoint lxsize (x, y) =
  assert (lxsize > x && x >= 0 && y >= 0 `blame` (lxsize, x, y)) $
  x + y * lxsize

-- | Conversion from @Point@ to cartesian coordinates.
fromPoint :: X -> Point -> PointXY
fromPoint lxsize loc =
  assert (loc >= 0 `blame` (lxsize, loc)) $
  (loc `rem` lxsize, loc `quot` lxsize)

-- | The top-left corner location of the level.
origin :: Point
origin = 0

-- | The distance between two points in the chessboard metric.
chessDist :: X -> Point -> Point -> Int
chessDist lxsize loc0 loc1
  | (x0, y0) <- fromPoint lxsize loc0, (x1, y1) <- fromPoint lxsize loc1 =
  chessDistXY $ VectorXY (x1 - x0, y1 - y0)

-- | Checks whether two points are adjacent on the map
-- (horizontally, vertically or diagonally).
-- A position is also considered adjacent to itself.
adjacent :: X -> Point -> Point -> Bool
adjacent lxsize s t = chessDist lxsize s t <= 1

-- | Returns the 8, or less, surrounding locations of a given location.
vicinity :: X -> Y -> Point -> [Point]
vicinity lxsize lysize loc =
  map (toPoint lxsize) $
    vicinityXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint lxsize loc
