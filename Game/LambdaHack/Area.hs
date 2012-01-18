-- | Rectangular areas of levels and their operations.
module Game.LambdaHack.Area
  ( Area, vicinityXY, vicinityCardinalXY, inside, fromTo
  , normalize, normalizeArea, grid, validArea, trivialArea, expand
  ) where

import qualified Data.List as L

import Game.LambdaHack.Geometry
import Game.LambdaHack.Utils.Assert

-- | The type of areas. The bottom left and the top right points.
type Area = (X, Y, X, Y)

-- | All (8 at most) closest neighbours of a point within an area.
vicinityXY :: Area      -- ^ limit the search to this area
           -> (X, Y)    -- ^ location to find neighbors of
           -> [(X, Y)]
vicinityXY area xy =
  [ res | dxy <- movesXY, let res = shiftXY xy dxy, inside res area ]

-- | All (4 at most) cardinal direction neighbours of a point within an area.
vicinityCardinalXY :: Area  -- ^ limit the search to this area
           -> (X, Y)        -- ^ location to find neighbors of
           -> [(X, Y)]
vicinityCardinalXY area xy =
  [ res | dxy <- movesCardinalXY, let res = shiftXY xy dxy, inside res area ]

-- | Checks that a point belongs to an area.
inside :: (X, Y) -> Area -> Bool
inside (x, y) (x0, y0, x1, y1) =
  x1 >= x && x >= x0 && y1 >= y && y >= y0

-- | A list of all points on a straight vertical or horizontal line
-- between two points. Fails if no such line exists.
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

-- | Sort the collection of two points, in the derived lexicographic order.
normalize :: ((X, Y), (X, Y)) -> ((X, Y), (X, Y))
normalize (a, b) | a <= b    = (a, b)
                 | otherwise = (b, a)

-- | Sort the corners of an area so that the bottom left is the first point.
normalizeArea :: Area -> Area
normalizeArea (x0, y0, x1, y1) = (min x0 x1, min y0 y1, max x0 x1, max y0 y1)

-- | Divide uniformly a larger area into the given number of smaller areas.
grid :: (X, Y) -> Area -> [((X, Y), Area)]
grid (nx, ny) (x0, y0, x1, y1) =
  let xd = x1 - x0
      yd = y1 - y0
      -- Make sure that in caves not filled with rock, there is a passage
      -- across the cave, even if a single room blocks most of the cave.
      xborder = if nx == 1 then 3 else 2
      yborder = if ny == 1 then 3 else 2
  in [ ((x, y), (x0 + (xd * x `div` nx) + xborder,
                 y0 + (yd * y `div` ny) + yborder,
                 x0 + (xd * (x + 1) `div` nx) - xborder,
                 y0 + (yd * (y + 1) `div` ny) - yborder))
     | x <- [0..nx-1], y <- [0..ny-1] ]

-- | Checks if it's an area with at least one field.
validArea :: Area -> Bool
validArea (x0, y0, x1, y1) = x0 <= x1 && y0 <= y1

-- | Checks if it's an area with exactly one field.
trivialArea :: Area -> Bool
trivialArea (x0, y0, x1, y1) = x0 == x1 && y0 == y1

-- | Enlarge (or shrink) the given area on all fours sides by the amount.
expand :: Area -> Int -> Area
expand (x0, y0, x1, y1) k = (x0 - k, y0 - k, x1 + k, y1 + k)
