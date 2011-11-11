module Game.LambdaHack.Loc
  ( Loc, toLoc, fromLoc, trLoc, zeroLoc, distance, adjacent, surroundings
  ) where

import Game.LambdaHack.Geometry
import Game.LambdaHack.Utils.Assert

-- Loc is a positivie integer for speed and to enforce the use of wrappers
-- (we don't want newtype to avoid the trouble with using EnumMap
-- in place of IntMap, etc.). We represent the screen as a linear framebuffer,
-- when Loc is an Int offset counted from the first cell.
-- We do bounds check for the X size ASAP and each subsequent
-- array access performs another check, effectively for Y size.
-- After dungeon is generated (using (X, Y) points, not Loc), Locs are used
-- mainly as keys and not constructed often, so the performance will improve
-- due to smaller save files, IntMaps and cheaper array indexing,
-- including cheaper bounds checks.
type Loc = Int

toLoc :: X -> (X, Y) -> Loc
toLoc lxsize (x, y) =
  assert (lxsize > x && x >= 0 && y >= 0 `blame` (lxsize, x, y)) $
  x + y * lxsize

fromLoc :: X -> Loc -> (X, Y)
fromLoc lxsize loc =
  assert (loc >= 0 `blame` (lxsize, loc)) $
  (loc `rem` lxsize, loc `quot` lxsize)

-- | Translation by a vector, where dx and dy can be negative.
trLoc :: X -> Loc -> (X, Y) -> Loc
trLoc lxsize loc (dx, dy) =
  assert (loc >= 0 && res >= 0 `blame` (lxsize, loc, (dx, dy))) $
  res
   where res = loc + dx + dy * lxsize

zeroLoc :: Loc
zeroLoc = 0

-- | The distance between two points in the metric with diagonal moves.
distance :: X -> Loc -> Loc -> Int
distance lxsize loc0 loc1
  | (x0, y0) <- fromLoc lxsize loc0, (x1, y1) <- fromLoc lxsize loc1 =
  lenXY (x1 - x0, y1 - y0)

-- | Return whether two locations are adjacent on the map
-- (horizontally, vertically or diagonally).
-- A position is also considered adjacent to itself.
adjacent :: X -> Loc -> Loc -> Bool
adjacent lxsize s t = distance lxsize s t <= 1

-- | Return the 8, or less, surrounding locations of a given location.
surroundings :: X -> Y -> Loc -> [Loc]
surroundings lxsize lysize loc | (x, y) <- fromLoc lxsize loc =
  [ toLoc lxsize (x + dx, y + dy)
  | (dx, dy) <- movesXY,
    x + dx >= 0 && x + dx < lxsize &&
    y + dy >= 0 && y + dy < lysize ]
