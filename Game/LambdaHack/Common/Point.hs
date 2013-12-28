{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Common.Point
  ( Point, toPoint, fromPoint
  , chessDist, adjacent, vicinity, vicinityCardinal
  , inside, displacementXYZ, bla
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Int (Int32)
import qualified Data.Ix as Ix
import qualified System.Random as R

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
-- framebuffer and so the point coordinate aren't contiguous.
-- Similarly, only particular (beginnings of) lines can be represented
-- as a single array, not the whole screen.
newtype Point = Point Int
  deriving (Eq, Ord, Ix.Ix, Enum, R.Random)

instance Binary Point where
  put = put . (fromIntegral :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegral :: Int32 -> Int)) get

-- For debugging.
instance Show Point where
  show = show . fromPoint

-- | Conversion from cartesian coordinates to @Point@.
toPoint :: PointXY -> Point
toPoint pxy@(PointXY x y) =
  assert (x >= 0 && y >= 0 `blame` "invalid point coordinates"
                           `twith` (x, y))
  $ toEnum $ fromEnum pxy

-- | Conversion from @Point@ to cartesian coordinates.
fromPoint :: Point -> PointXY
fromPoint (Point p) =
  assert (p >= 0 `blame` "negative point value" `twith` p)
  $ toEnum $ fromEnum p

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

-- | Calculate the displacement vector from a position to another.
displacementXYZ :: Point -> Point -> VectorXY
displacementXYZ pos0 pos1
  | PointXY x0 y0 <- fromPoint pos0
  , PointXY x1 y1 <- fromPoint pos1 =
  VectorXY (x1 - x0) (y1 - y0)

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
