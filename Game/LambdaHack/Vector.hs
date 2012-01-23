-- | Basic operations on 2D vectors represented in an efficient,
-- but not unique, way.
module Game.LambdaHack.Vector
  ( Vector, toVector, shift, moves, movesWidth
  , euclidDistSq, diagonal, neg, towards
  ) where

import Data.Binary

import Game.LambdaHack.PointXY
import Game.LambdaHack.VectorXY
import Game.LambdaHack.Point
import Game.LambdaHack.Utils.Assert

-- | 2D vectors  represented as offsets in the linear framebuffer
-- indexed by 'Point'.
--
-- A newtype is used to prevent mixing up the type with @Point@ itself.
-- Note that the offset representations of a vector is usually not unique.
-- For vectors of lenth 1 in the chessboard metric, used to denote
-- geographical directions, if the level width and height are at least 3,
-- the representations are pairwise distinct.
newtype Vector = Vector Int
  deriving (Show, Eq)

instance Binary Vector where
  put (Vector dir) = put dir
  get = fmap Vector get

-- | Converts a vector in cartesian representation into @Vector@.
toVector :: X -> VectorXY -> Vector
toVector lxsize (VectorXY (x, y)) =
  Vector $ x + y * lxsize

-- | Converts a unit vector in cartesian representation into @Vector@.
toDir :: X -> VectorXY -> Vector
toDir lxsize v@(VectorXY (x, y)) =
  assert (lxsize >= 3 && chessDistXY v == 1 `blame` (lxsize, v)) $
  Vector $ x + y * lxsize

-- | Converts a unit vector in the offset representation
-- into the cartesian representation. Arbitrary vectors can't be
-- converted uniquely.
fromDir :: X -> Vector -> VectorXY
fromDir lxsize (Vector dir) =
  assert (lxsize >= 3 && chessDistXY res == 1 &&
          fst len1 + snd len1 * lxsize == dir
          `blame` (lxsize, dir, res)) $
  res
 where
  (x, y) = (dir `mod` lxsize, dir `div` lxsize)
  -- Pick the vector's canonical form of length 1:
  len1 = if x > 1
         then (x - lxsize, y + 1)
         else (x, y)
  res = VectorXY len1

-- | Translate a point by a vector.
--
-- Particularly simple and fast implementation in the linear representation.
shift :: Point -> Vector -> Point
shift loc (Vector dir) = loc + dir

-- | Vectors of all unit moves, clockwise, starting north-west.
moves :: X -> [Vector]
moves lxsize = map (toDir lxsize) movesXY

-- | Vectors of all unit moves, clockwise, starting north-west,
-- parameterized by level width.
movesWidth :: [X -> Vector]
movesWidth = map (flip toDir) movesXY

-- | Squared euclidean distance between two unit vectors.
euclidDistSq :: X -> Vector -> Vector -> Int
euclidDistSq lxsize dir0 dir1
  | VectorXY (x0, y0) <- fromDir lxsize dir0
  , VectorXY (x1, y1) <- fromDir lxsize dir1 =
  euclidDistSqXY $ VectorXY ((y1 - y0), (x1 - x0))

-- | Checks whether a unit vector is a diagonal direction,
-- as opposed to cardinal.
diagonal :: X -> Vector -> Bool
diagonal lxsize dir | VectorXY (x, y) <- fromDir lxsize dir =
  x * y /= 0

-- | Reverse an arbirary vector.
neg :: Vector -> Vector
neg (Vector dir) = Vector (-dir)

-- TODO: Perhaps produce all acceptable directions and let AI choose.
-- That would also eliminate the Doubles.
-- | Given two distinct locations, determine the direction (a unit vector)
-- in which one should move from the first in order to get closer to the second.
-- Ignores obstacles. Of several equally good directions
-- (in the chessboard metric) it picks one of those that visually
-- (in the euclidean metric) are maximally straightforward.
towards :: X -> Point -> Point -> Vector
towards lxsize loc0 loc1
  | (x0, y0) <- fromPoint lxsize loc0, (x1, y1) <- fromPoint lxsize loc1 =
  assert (loc0 /= loc1 `blame` (loc0, loc1, x0, y0)) $
  let dx = x1 - x0
      dy = y1 - y0
      angle :: Double
      angle = atan (fromIntegral dy / fromIntegral dx) / (pi / 2)
      dxy | angle <= -0.75 = (0, -1)
          | angle <= -0.25 = (1, -1)
          | angle <= 0.25  = (1, 0)
          | angle <= 0.75  = (1, 1)
          | angle <= 1.25  = (0, 1)
          | otherwise =
              assert `failure` (lxsize, loc0, loc1, (x0, y0), (x1, y1))
  in if dx >= 0
     then toDir lxsize $ VectorXY dxy
     else neg (toDir lxsize $ VectorXY dxy)
