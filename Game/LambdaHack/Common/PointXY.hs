-- | Basic cartesian geometry operations on 2D points.
module Game.LambdaHack.Common.PointXY
  ( X, Y, PointXY(..), maxLevelDimExponent, maxLevelDim, maxVectorDim
  , fromTo, sortPointXY, blaXY, insideXY
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import Data.Int (Int32)

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

instance Enum PointXY where
  toEnum xy = PointXY (xy .&. maxLevelDim)
                      (unsafeShiftR xy maxLevelDimExponent)
  fromEnum (PointXY x y) = x + unsafeShiftL y maxLevelDimExponent

instance Binary PointXY where
  put = put . (fromIntegral :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegral :: Int32 -> Int)) get

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

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA). Includes the source point
-- and goes through the target point to infinity.
blaXY :: Int -> PointXY -> PointXY -> [PointXY]
blaXY eps (PointXY x0 y0) (PointXY x1 y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      bw = balancedWord p q (eps `mod` max 1 q)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in map (uncurry PointXY) $ walk bw (x0, y0)

-- | Checks that a point belongs to an area.
insideXY :: PointXY -> (X, Y, X, Y) -> Bool
insideXY (PointXY x y) (x0, y0, x1, y1) =
  x1 >= x && x >= x0 && y1 >= y && y >= y0
