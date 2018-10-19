{-# LANGUAGE DeriveGeneric #-}
-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Common.Point
  ( X, Y, Point(..)
  , pindex, punindex, chessDist, euclidDistSq, adjacent, bla, fromTo
  , originPoint
  , speedupHackXSize
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , blaXY, balancedWord
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Int (Int32)
import GHC.Generics (Generic)

-- | This is a hackily hardcoded maximal level width, for speed,
-- to be replaced by some clever approach, e.g., a common library
-- on which content depends, on which engine main code depends,
-- taking the size from rules content. @IORef@ is not clever enough
-- because reading it allocates too much.
speedupHackXSize :: Int
speedupHackXSize = 80

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

instance Enum Point where
  {-# INLINE fromEnum #-}
  fromEnum p@Point{..} =
    let xsize = speedupHackXSize
    in
#ifdef WITH_EXPENSIVE_ASSERTIONS
       assert (px >= 0 && py >= 0 && px < xsize
              `blame` "invalid point coordinates"
              `swith` (px, py))
#endif
         (pindex xsize p)
  {-# INLINE toEnum #-}
  toEnum = let xsize = speedupHackXSize
           in punindex xsize

-- Note that @Ord@ on @Int@ is not monotonic wrt @Ord@ on @Point@.
-- We need to keep it that way, because we want close xs to have close indexes,
-- e.g., adjacent points in line to have adjacent enumerations,
-- because some of the screen layout and most of processing is line-by-line.
-- Consequently, one can use EM.fromAscList on @(1, 8)..(10, 8)@, but not on
-- @(1, 7)..(10, 9)@.
pindex :: X -> Point -> Int
{-# INLINE pindex #-}
pindex xsize (Point x y) = x + y * xsize

punindex :: X -> Int -> Point
{-# INLINE punindex #-}
punindex xsize n = let (py, px) = n `quotRem` xsize
                   in Point{..}

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
-- to the edge of the level. Gives @Nothing@ if the points are equal.
-- The target is given as @Point@ to permit aiming out of the level,
-- e.g., to get uniform distributions of directions for explosions
-- close to the edge of the level.
bla :: X -> Y -> Int -> Point -> Point -> Maybe [Point]
bla rXmax rYmax eps source target =
  if source == target then Nothing
  else Just $
    let inBounds p@(Point x y) =
          rXmax > x && x >= 0 && rYmax > y && y >= 0 && p /= source
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
