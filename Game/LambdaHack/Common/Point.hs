{-# LANGUAGE DeriveGeneric #-}
-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Common.Point
  ( X, Y, Point(..), maxLevelDimExponent
  , chessDist, euclidDistSq, adjacent, bla, fromTo
  , originPoint
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , maxLevelDim, blaXY, balancedWord
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import Data.Int (Int32)
import GHC.Generics (Generic)

-- | Spacial dimension for points and vectors.
type X = Int

-- | Spacial dimension for points and vectors.
type Y = Int

-- | 2D points in cartesian representation. Coordinates grow to the right
-- and down, so that the (0, 0) point is in the top-left corner of the screen.
-- Coordinates are never negative.
data Point = Point
  { px :: X
  , py :: Y
  }
  deriving (Eq, Ord, Generic)

instance Show Point where
  show (Point x y) = show (x, y)

instance Binary Point where
  put = put . (fromIntegral :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegral :: Int32 -> Int)) get

-- This conversion cannot be used for PointArray indexing,
-- because it is not contiguous --- we don't know the horizontal
-- width of the levels nor of the screen.
-- The conversion is implemented mainly for @EnumMap@ and @EnumSet@.
-- Note that the conversion is not monotonic wrt the natural @Ord@ instance,
-- because we want adjacent points in line to have adjacent enumerations,
-- because some of the screen layout and most of processing is line-by-line.
-- Consequently, one can use EM.fromAscList on @(1, 8)..(10, 8)@, but not on
-- @(1, 7)..(10, 9)@.
instance Enum Point where
  fromEnum (Point x y) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
    assert (x >= 0 && y >= 0 && x <= maxLevelDim && y <= maxLevelDim
            `blame` "invalid point coordinates"
            `swith` (x, y))
#endif
    (x + unsafeShiftL y maxLevelDimExponent)
  toEnum n = Point (n .&. maxLevelDim) (unsafeShiftR n maxLevelDimExponent)

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

-- | The distance between two points in the chessboard metric.
chessDist :: Point -> Point -> Int
chessDist (Point x0 y0) (Point x1 y1) = max (abs (x1 - x0)) (abs (y1 - y0))

-- | Squared euclidean distance between two points.
euclidDistSq :: Point -> Point -> Int
euclidDistSq (Point x0 y0) (Point x1 y1) =
  (x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int)

-- | Checks whether two points are adjacent on the map
-- (horizontally, vertically or diagonally).
adjacent :: Point -> Point -> Bool
{-# INLINE adjacent #-}
adjacent s t = chessDist s t == 1

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA).
-- Skips the source point and goes through the second point
-- to the edge of the level. GIves @Nothing@ if the points are equal.
-- The target is given as @Point@ to permit aiming out of the level,
-- e.g., to get uniform distributions of directions for explosions
-- close to the edge of the level.
bla :: X -> Y -> Int -> Point -> Point -> Maybe [Point]
bla lxsize lysize eps source target =
  if source == target then Nothing
  else Just $
    let inBounds p@(Point x y) =
          lxsize > x && x >= 0 && lysize > y && y >= 0 && p /= source
    in takeWhile inBounds $ tail $ blaXY eps source target

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA). Includes the source point
-- and goes through the target point to infinity.
blaXY :: Int -> Point -> Point -> [Point]
blaXY eps (Point x0 y0) (Point x1 y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      bw = balancedWord p q (eps `mod` max 1 q)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in map (uncurry Point) $ walk bw (x0, y0)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

-- | A list of all points on a straight vertical or straight horizontal line
-- between two points. Fails if no such line exists.
fromTo :: Point -> Point -> [Point]
fromTo (Point x0 y0) (Point x1 y1) =
 let fromTo1 :: Int -> Int -> [Int]
     fromTo1 z0 z1
       | z0 <= z1  = [z0..z1]
       | otherwise = [z0,z0-1..z1]
     result
       | x0 == x1 = map (Point x0) (fromTo1 y0 y1)
       | y0 == y1 = map (`Point` y0) (fromTo1 x0 x1)
       | otherwise = error $ "diagonal fromTo"
                             `showFailure` ((x0, y0), (x1, y1))
 in result

originPoint :: Point
originPoint = Point 0 0
