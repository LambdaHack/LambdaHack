-- | Basic cartesian geometry operations on 2D points.
module Game.LambdaHack.Common.PointXY
  ( X, Y, PointXY(..)
  , maxLevelDimExponent, maxLevelDim, maxVectorDim
  , fromTo, sortPointXY, insideXY
  ) where

import Control.Exception.Assert.Sugar

-- | Spacial dimension for points and vectors.
type X = Int

-- | Spacial dimension for points and vectors.
type Y = Int

-- | 2D points in cartesian representation. Coordinates grow to the right
-- and down, so that the (0, 0) point is in the top-left corner of the screen.
-- Coordinates are never negative.
--
-- Note: If we used this instead of @Point@, we'd need, e.g.,
-- an extra addition per FOV point and per AI speculative move,
-- plus some possible extra memory accesses to the individual coordinates.
-- Even more serious is an extra addition and access per EnumMap lookup,
-- though in the computation-intensive cases of FOV and AI, the extra
-- operations were already there, performed before lookup.
data PointXY = PointXY
  { px :: !X
  , py :: !Y
  }
  deriving (Eq, Ord)

instance Show PointXY where
  show (PointXY x y) = show (x, y)

-- | The maximum number of bits for level X and Y dimension (16).
-- The value is chosen to support architectures with 32-bit Ints.
maxLevelDimExponent :: Int
{-# INLINE maxLevelDimExponent #-}
maxLevelDimExponent = 16

-- | Maximal supported level X and Y dimension. Not checked anywhere.
-- The value is chosen to support architectures with 32-bit Ints.
maxLevelDim :: Int
{-# INLINE maxLevelDim #-}
maxLevelDim = 2 ^ maxLevelDimExponent - 1

-- | Maximal supported vector X and Y coordinates.
maxVectorDim :: Int
{-# INLINE maxVectorDim #-}
maxVectorDim = 2 ^ (maxLevelDimExponent - 1) - 1

-- TODO: can be implemented in Point entirely.
-- | A list of all points on a straight vertical or straight horizontal line
-- between two points. Fails if no such line exists.
fromTo :: PointXY -> PointXY -> [PointXY]
fromTo (PointXY x0 y0) (PointXY x1 y1) =
 let result
       | x0 == x1 = map (\ y -> PointXY x0 y) (fromTo1 y0 y1)
       | y0 == y1 = map (\ x -> PointXY x y0) (fromTo1 x0 x1)
       | otherwise = assert `failure` "diagonal fromTo"
                            `twith` ((x0, y0), (x1, y1))
 in result

fromTo1 :: Int -> Int -> [Int]
fromTo1 x0 x1
  | x0 <= x1  = [x0..x1]
  | otherwise = [x0,x0-1..x1]

-- | Sort the sequence of two points, in the derived lexicographic order.
sortPointXY :: (PointXY, PointXY) -> (PointXY, PointXY)
sortPointXY (a, b) | a <= b    = (a, b)
                   | otherwise = (b, a)

-- | Checks that a point belongs to an area.
insideXY :: PointXY -> (X, Y, X, Y) -> Bool
insideXY (PointXY x y) (x0, y0, x1, y1) =
  x1 >= x && x >= x0 && y1 >= y && y >= y0
