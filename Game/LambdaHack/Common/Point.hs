{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Common.Point
  ( Point, toPoint, fromPoint
  , chessDist, adjacent, vicinity, vicinityCardinal, inside, bla
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import Data.Int (Int32)

import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.VectorXY

-- | The type of positions on the 2D level map, heavily optimized.
--
-- We represent the (level map on the) screen as a linear
-- framebuffer with very long lines, where @Point@ is an @Int@
-- offset counted from the first cell. We do bounds checks wrt
-- our smaller, real screen framebuffer on each array access.
-- After dungeon is generated (using @PointXY@, not @Point@),
-- and converted to the @Point@ representation, points are used
-- mainly as keys and not constructed often, so the performance will improve
-- due to smaller save files, a bit less memory accesses and cheaper array
-- and map indexing, including cheaper bounds checks.
--
-- Note: do not iterate over the real screen by incrementing the point,
-- since our screen is a small fraction of the represented abstract
-- framebuffer and so the screen coordinates aren't contiguous.
-- Similarly, only separate lines can be represented as arrays,
-- not the whole screen.
newtype Point = Point Int
  deriving (Eq, Ord, Enum)

instance Binary Point where
  put = put . (fromIntegral :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegral :: Int32 -> Int)) get

-- For debugging.
instance Show Point where
  show = show . fromPoint

-- | Conversion from cartesian coordinates to @Point@.
toPoint :: PointXY -> Point
toPoint pxy@(PointXY x y) =
  let fromEnumXY (PointXY x1 y1) = x1 + unsafeShiftL y1 maxLevelDimExponent
  in assert (x >= 0 && y >= 0 `blame` "invalid point coordinates"
                           `twith` (x, y))
     $ toEnum $ fromEnumXY pxy

-- | Conversion from @Point@ to cartesian coordinates.
fromPoint :: Point -> PointXY
fromPoint (Point p) =
  let toEnumXY xy = PointXY (xy .&. maxLevelDim)
                            (unsafeShiftR xy maxLevelDimExponent)
  in assert (p >= 0 `blame` "negative point value" `twith` p)
     $ toEnumXY $ fromEnum p

-- | The distance between two points in the chessboard metric.
chessDist :: Point -> Point -> Int
chessDist pos0 pos1
  | PointXY x0 y0 <- fromPoint pos0
  , PointXY x1 y1 <- fromPoint pos1 =
  chessDistXY $ VectorXY (x1 - x0) (y1 - y0)

-- | Checks whether two points are adjacent on the map
-- (horizontally, vertically or diagonally).
adjacent :: Point -> Point -> Bool
adjacent s t = chessDist s t == 1

-- | Returns the 8, or less, surrounding positions of a given position.
vicinity :: X -> Y -> Point -> [Point]
vicinity lxsize lysize p =
  map toPoint $
    vicinityXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint p

-- | Returns the 4, or less, surrounding positions in cardinal directions
-- from a given position.
vicinityCardinal :: X -> Y -> Point -> [Point]
vicinityCardinal lxsize lysize p =
  map toPoint $
    vicinityCardinalXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint p

-- | Checks that a point belongs to an area.
inside :: Point -> (X, Y, X, Y) -> Bool
inside p = insideXY $ fromPoint p

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA).
-- Skips the source point and goes through the second point
-- to the edge of the level. GIves @Nothing@ if the points are equal.
bla :: X -> Y -> Int -> Point -> Point -> Maybe [Point]
bla _ _ _ source target | source == target = Nothing
bla lxsize lysize eps source target = Just $
  let s = fromPoint source
      e = fromPoint target
      inBounds p@(PointXY x y) =
        lxsize > x && x >= 0 && lysize > y && y >= 0 && p /= s
  in map toPoint $ takeWhile inBounds $ tail $ blaXY eps s e

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
