-- | Locations on the level map.
module Game.LambdaHack.Loc
  ( Loc, toLoc, fromLoc, trLoc, zeroLoc, distance, adjacent, vicinity
  ) where

import Game.LambdaHack.Geometry
import Game.LambdaHack.Area
import Game.LambdaHack.Utils.Assert

-- | The type of level map locations, heavily optimized.
--
-- We represent the (level map on the) screen as a linear framebuffer,
-- where @Loc@ is an @Int@ offset counted from the first cell.
-- We do bounds check for the X size ASAP and each subsequent
-- array access performs another check, effectively for Y size.
-- After dungeon is generated (using (X, Y) points, not @Loc@),
-- and converted to the @Loc@ representation, locations are used
-- mainly as keys and not constructed often, so the performance will improve
-- due to smaller save files, the use of @IntMap@ and cheaper array indexing,
-- including cheaper bounds checks.
-- We don't use a newtype to avoid the trouble with using @EnumMap@
-- in place of @IntMap@, etc.
type Loc = Int

-- | Conversion from cartesian coordinates to @Loc@.
toLoc :: X -> (X, Y) -> Loc
toLoc lxsize (x, y) =
  assert (lxsize > x && x >= 0 && y >= 0 `blame` (lxsize, x, y)) $
  x + y * lxsize

-- | Conversion from @Loc@ to cartesian coordinates.
fromLoc :: X -> Loc -> (X, Y)
fromLoc lxsize loc =
  assert (loc >= 0 `blame` (lxsize, loc)) $
  (loc `rem` lxsize, loc `quot` lxsize)

-- | Translation by a vector.
trLoc :: X -> Loc -> (X, Y) -> Loc
trLoc lxsize loc (dx, dy) =
  -- Vector coordinates can be negative, but locs are always positive.
  assert (loc >= 0 && res >= 0 `blame` (lxsize, loc, (dx, dy))) $
  res
   where res = loc + dx + dy * lxsize

-- | The top-left corner location of the level.
zeroLoc :: Loc
zeroLoc = 0

-- | The distance between two points in the metric with cheap diagonal moves.
distance :: X -> Loc -> Loc -> Int
distance lxsize loc0 loc1
  | (x0, y0) <- fromLoc lxsize loc0, (x1, y1) <- fromLoc lxsize loc1 =
  lenXY (x1 - x0, y1 - y0)

-- | Checks whether two locations are adjacent on the map
-- (horizontally, vertically or diagonally).
-- A position is also considered adjacent to itself.
adjacent :: X -> Loc -> Loc -> Bool
adjacent lxsize s t = distance lxsize s t <= 1

-- | Returns the 8, or less, surrounding locations of a given location.
vicinity :: X -> Y -> Loc -> [Loc]
vicinity lxsize lysize loc =
  map (toLoc lxsize) $
    vicinityXY (0, 0, lxsize - 1, lysize - 1) $
      fromLoc lxsize loc
