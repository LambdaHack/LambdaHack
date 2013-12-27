{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Common.Point
  ( Point, toPoint, showPoint
  , origin, chessDist, adjacent, vicinity, vicinityCardinal
  , inside, displacementXYZ, bla
  ) where

import Data.Binary
import qualified Data.Ix as Ix
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Random as R

import Control.Exception.Assert.Sugar
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.VectorXY

-- | The type of positions on the 2D level map, heavily optimized.
--
-- We represent the (level map on the) screen as a linear framebuffer,
-- where @Point@ is an @Int@ offset counted from the first cell.
-- We do bounds check for the X size whenever we convert
-- between representations and each subsequent array access
-- performs another check, effectively for Y size.
-- After dungeon is generated (using @PointXY@, not @Point@),
-- and converted to the @Point@ representation, points are used
-- mainly as keys and not constructed often, so the performance will improve
-- due to smaller save files, the use of @EnumMap@ and cheaper array indexing,
-- including cheaper bounds checks.
newtype Point = Point Int
  deriving (Eq, Ord, Ix.Ix, Enum, R.Random)

instance Binary Point where
  put (Point n) = put n
  get = fmap Point get

-- For debugging.
instance Show Point where
  show (Point n) = show n

-- | Print a point as a tuple of cartesian coordinates.
showPoint :: X -> Point -> Text
showPoint lxsize = T.pack . show . fromPoint lxsize

-- | Conversion from cartesian coordinates to @Point@.
toPoint :: X -> PointXY -> Point
toPoint lxsize (PointXY x y) =
  assert (lxsize > x && x >= 0 && y >= 0 `blame` "invalid point coordinates"
                                         `twith` (lxsize, x, y))
  $ Point $ x + y * lxsize

-- | Conversion from @Point@ to cartesian coordinates.
fromPoint :: X -> Point -> PointXY
fromPoint lxsize (Point p) =
  assert (p >= 0 `blame` "negative point value" `twith` (lxsize, p))
  $ PointXY (p `rem` lxsize) (p `quot` lxsize)

-- | The top-left corner position of the level.
origin :: Point
origin = Point 0

-- | The distance between two points in the chessboard metric.
chessDist :: X -> Point -> Point -> Int
chessDist lxsize pos0 pos1
  | PointXY x0 y0 <- fromPoint lxsize pos0
  , PointXY x1 y1 <- fromPoint lxsize pos1 =
  chessDistXY $ VectorXY (x1 - x0, y1 - y0)

-- | Checks whether two points are adjacent on the map
-- (horizontally, vertically or diagonally).
adjacent :: X -> Point -> Point -> Bool
adjacent lxsize s t = chessDist lxsize s t == 1

-- | Returns the 8, or less, surrounding positions of a given position.
vicinity :: X -> Y -> Point -> [Point]
vicinity lxsize lysize p =
  map (toPoint lxsize) $
    vicinityXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint lxsize p

-- | Returns the 4, or less, surrounding positions in cardinal directions
-- from a given position.
vicinityCardinal :: X -> Y -> Point -> [Point]
vicinityCardinal lxsize lysize p =
  map (toPoint lxsize) $
    vicinityCardinalXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint lxsize p

-- | Checks that a point belongs to an area.
inside :: X -> Point -> (X, Y, X, Y) -> Bool
inside lxsize p = insideXY $ fromPoint lxsize p

-- | Calculate the displacement vector from a position to another.
displacementXYZ :: X -> Point -> Point -> VectorXY
displacementXYZ lxsize pos0 pos1
  | PointXY x0 y0 <- fromPoint lxsize pos0
  , PointXY x1 y1 <- fromPoint lxsize pos1 =
  VectorXY (x1 - x0, y1 - y0)

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA).
-- Skips the source point and goes through the second point
-- to the edge of the level. GIves @Nothing@ if the points are equal.
bla :: X -> Y -> Int -> Point -> Point -> Maybe [Point]
bla _ _ _ source target | source == target = Nothing
bla lxsize lysize eps source target = Just $
  let s = fromPoint lxsize source
      e = fromPoint lxsize target
      inBounds p@(PointXY x y) =
        lxsize > x && x >= 0 && lysize > y && y >= 0 && p /= s
  in L.map (toPoint lxsize) $ L.takeWhile inBounds $ L.tail $ blaXY eps s e
