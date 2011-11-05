module Geometry
  ( Time, VDir(..), X, Y, Loc, Dir, toLoc, fromLoc, trLoc, zeroLoc
  , Area, towards, distance, distanceDir, adjacent, surroundings, diagonal, shift
  , neg, moves, up, down, left, right, upleft, upright, downleft, downright
  , neighbors, fromTo, normalize, normalizeArea, grid, shiftXY
  ) where

import qualified Data.List as L
import Data.Binary

import Utils.Assert

-- | Game time in turns. (Placement in module Geometry is not ideal.)
type Time = Int

-- | Vertical directions.
data VDir = Up | Down
  deriving (Eq, Ord, Show, Enum, Bounded)

type X = Int
type Y = Int

-- Loc is a positivie integer for speed and to enforce the use of wrappers
-- (we don't want newtype to avoid the trouble with using EnumMap
-- in place of IntMap, etc.). We do bounds check for the X size ASAP
-- and each subsequent array access performs a checlk, effectively for Y size.
-- After dungeon is generated (using (X, Y), not Loc), Locs are used
-- mainly as keys and not constructed often, so the performance will improve
-- due to smaller save files, IntMaps and cheaper array indexing,
-- including cheaper bounds check.
type Loc = Int

-- | Construct a location.
toLoc :: X -> (X, Y) -> Loc
toLoc lxsize (x, y) =
  assert (lxsize > x && x >= 0 && y >= 0 `blame` (lxsize, x, y)) $
  x + y * lxsize

fromLoc :: X -> Loc -> (X, Y)
fromLoc lxsize loc =
  assert (lxsize > 0 && loc >= 0 `blame` (lxsize, loc)) $
  (loc `rem` lxsize, loc `quot` lxsize)

trLoc :: X -> Loc -> (X, Y) -> Loc
trLoc lxsize loc (dx, dy) =
  assert (lxsize > 0 && loc >= 0 && res >= 0 `blame` (lxsize, loc, (dx, dy))) $
  res
   where res = loc + dx + dy * lxsize

zeroLoc :: Loc
zeroLoc = 0

newtype Dir = Dir (X, Y) deriving (Show, Eq)

instance Binary Dir where
  put (Dir xy) = put xy
  get = fmap Dir get

type Area = (X, Y, X, Y)

-- | Given two locations, determine the direction in which one should
-- move from the first in order to get closer to the second. Does not
-- pay attention to obstacles at all.
towards :: X -> (Loc, Loc) -> Dir
towards lxsize (loc0, loc1)
  | (x0, y0) <- fromLoc lxsize loc0, (x1, y1) <- fromLoc lxsize loc1 =
  let dx = x1 - x0
      dy = y1 - y0
      angle :: Double
      angle = atan (fromIntegral dy / fromIntegral dx) / (pi / 2)
      dir | angle <= -0.75 = (0, -1)
          | angle <= -0.25 = (1, -1)
          | angle <= 0.25  = (1, 0)
          | angle <= 0.75  = (1, 1)
          | angle <= 1.25  = (0, 1)
          | otherwise      = (0, 0)
  in  if dx >= 0 then Dir dir else neg (Dir dir)

-- | Get the squared distance between two locations.
distance :: X -> (Loc, Loc) -> Int
distance lxsize (loc0, loc1)
  | (x0, y0) <- fromLoc lxsize loc0, (x1, y1) <- fromLoc lxsize loc1 =
  let square a = a * a
  in square (y1 - y0) + square (x1 - x0)

distanceDir :: (Dir, Dir) -> Int
distanceDir (Dir (x0, y0), Dir (x1, y1)) =
  let square a = a * a
  in square (y1 - y0) + square (x1 - x0)

-- | Return whether two locations are adjacent on the map
-- (horizontally, vertically or diagonally). Currrently, a
-- position is also considered adjacent to itself.
adjacent :: X -> Loc -> Loc -> Bool
adjacent lxsize s t = distance lxsize (s, t) <= 2

-- | Return the 8 surrounding locations of a given location.
surroundings :: X -> Y -> Loc -> [Loc]
surroundings lxsize lysize loc | (x, y) <- fromLoc lxsize loc =
  [ toLoc lxsize (x + dx, y + dy)
  | Dir (dx, dy) <- moves,
    x + dx >= 0 && x + dx < lxsize &&
    y + dy >= 0 && y + dy < lysize ]

diagonal :: Dir -> Bool
diagonal (Dir (x, y)) = x * y /= 0

-- | Move one square in the given direction.
shift :: Loc -> X -> Dir -> Loc
shift loc lxsize (Dir (dx, dy)) =
  assert (lxsize > 0 && loc >= 0 && res >= 0 `blame` (lxsize, loc, (dx, dy))) $
  res
   where res = loc + dx + dy * lxsize

-- | Invert a direction (vector).
neg :: Dir -> Dir
neg (Dir (x, y)) = Dir (-x, -y)

-- | Get the vectors of all the moves, clockwise, starting north-west.
moves :: [Dir]
moves =
  map Dir [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

shiftDir :: Dir -> Dir -> Dir
shiftDir (Dir (x0, y0)) (Dir (x1, y1)) = Dir (x0 + x1, y0 + y1)

shiftXY :: (X, Y) -> (X, Y) -> (X, Y)
shiftXY (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

up, down, left, right :: Dir
upleft, upright, downleft, downright :: Dir
upleft    = up   `shiftDir` left
upright   = up   `shiftDir` right
downleft  = down `shiftDir` left
downright = down `shiftDir` right
up        = Dir (0, -1)
down      = Dir (0, 1)
left      = Dir (-1, 0)
right     = Dir (1, 0)

neighbors :: Area ->        {- size limitation -}
             (X, Y) ->      {- location to find neighbors of -}
             [(X, Y)]
neighbors area xy =
  let cs = [ xy `shiftXY` (dx, dy)
           | dy <- [-1..1], dx <- [-1..1], (dx + dy) `mod` 2 == 1 ]
  in  L.filter (`inside` area) cs

inside :: (X, Y) -> Area -> Bool
inside (x, y) (x0, y0, x1, y1) =
  x1 >= x && x >= x0 && y1 >= y && y >= y0

fromTo :: (X, Y) -> (X, Y) -> [(X, Y)]
fromTo (x0, y0) (x1, y1) =
 let result
       | x0 == x1 = L.map (\ y -> (x0, y)) (fromTo1 y0 y1)
       | y0 == y1 = L.map (\ x -> (x, y0)) (fromTo1 x0 x1)
       | otherwise = assert `failure` ((x0, y0), (x1, y1))
 in result

fromTo1 :: Int -> Int -> [Int]
fromTo1 x0 x1
  | x0 <= x1  = [x0..x1]
  | otherwise = [x0,x0-1..x1]

normalize :: ((X, Y), (X, Y)) -> ((X, Y), (X, Y))
normalize (a, b) | a <= b    = (a, b)
                 | otherwise = (b, a)

normalizeArea :: Area -> Area
normalizeArea (x0, y0, x1, y1) = (min x0 x1, min y0 y1, max x0 x1, max y0 y1)

grid :: (X, Y) -> Area -> [((X, Y), Area)]
grid (nx, ny) (x0, y0, x1, y1) =
  let yd = y1 - y0
      xd = x1 - x0
  in [ ((x, y), (x0 + (xd * x `div` nx),
                 y0 + (yd * y `div` ny),
                 x0 + (xd * (x + 1) `div` nx - 1),
                 y0 + (yd * (y + 1) `div` ny - 1)))
     | x <- [0..nx-1], y <- [0..ny-1] ]
