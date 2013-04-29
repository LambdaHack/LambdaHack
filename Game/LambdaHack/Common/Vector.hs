-- | Basic operations on 2D vectors represented in an efficient,
-- but not unique, way.
module Game.LambdaHack.Common.Vector
  ( Vector, toVector, toDir, shift, shiftBounded, moves
  , isUnit, euclidDistSq, diagonal, neg, towards, displacement
  , displacePath, shiftPath
  ) where

import Data.Binary

import Game.LambdaHack.Common.Area
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Common.VectorXY

-- | 2D vectors  represented as offsets in the linear framebuffer
-- indexed by 'Point'.
--
-- A newtype is used to prevent mixing up the type with @Point@ itself.
-- Note that the offset representations of a vector is usually not unique.
-- E.g., for vectors of length 1 in the chessboard metric, used to denote
-- geographical directions, the representations are pairwise distinct
-- if and only if the level width and height are at least 3.
newtype Vector = Vector Int
  deriving (Eq, Ord, Read)

instance Binary Vector where
  put (Vector dir) = put dir
  get = fmap Vector get

-- For debugging.
instance Show Vector where
  show (Vector n) = show n

-- | Converts a vector in cartesian representation into @Vector@.
toVector :: X -> VectorXY -> Vector
toVector lxsize (VectorXY (x, y)) =
  Vector $ x + y * lxsize

isUnitXY :: VectorXY -> Bool
isUnitXY v = chessDistXY v == 1

-- | Tells if a vector has length 1 in the chessboard metric.
isUnit ::  X -> Vector -> Bool
isUnit lxsize = isUnitXY . fromDir lxsize

-- | Converts a unit vector in cartesian representation into @Vector@.
toDir :: X -> VectorXY -> Vector
toDir lxsize v@(VectorXY (x, y)) =
  assert (lxsize >= 3 && isUnitXY v `blame` (lxsize, v)) $
  Vector $ x + y * lxsize

-- | Converts a unit vector in the offset representation
-- into the cartesian representation. Arbitrary vectors can't be
-- converted uniquely.
fromDir :: X -> Vector -> VectorXY
fromDir lxsize (Vector dir) =
  assert (lxsize >= 3 && isUnitXY res &&
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
shift p (Vector dir) = toEnum $ fromEnum p + dir

-- | Translate a point by a vector, but only if the result fits in an area.
shiftBounded :: X -> Area -> Point -> Vector -> Point
shiftBounded lxsize area loc dir =
  let res = shift loc dir
  in if inside lxsize res area then res else loc

-- | Vectors of all unit moves, clockwise, starting north-west.
moves :: X -> [Vector]
moves lxsize = map (toDir lxsize) movesXY

-- | Squared euclidean distance between two unit vectors.
euclidDistSq :: X -> Vector -> Vector -> Int
euclidDistSq lxsize dir0 dir1
  | VectorXY (x0, y0) <- fromDir lxsize dir0
  , VectorXY (x1, y1) <- fromDir lxsize dir1 =
  euclidDistSqXY $ VectorXY (x1 - x0, y1 - y0)

-- | Checks whether a unit vector is a diagonal direction,
-- as opposed to cardinal.
diagonal :: X -> Vector -> Bool
diagonal lxsize dir | VectorXY (x, y) <- fromDir lxsize dir =
  x * y /= 0

-- | Reverse an arbirary vector.
neg :: Vector -> Vector
neg (Vector dir) = Vector (-dir)

-- TODO: use bla for that
-- | Given a vector of arbitrary non-zero length, produce a unit vector
-- that points in the same direction (in the chessboard metric).
-- Of several equally good directions it picks one of those that visually
-- (in the euclidean metric) maximally align with the original vector.
normalize :: X -> VectorXY -> Vector
normalize lxsize v@(VectorXY (dx, dy)) =
  assert (dx /= 0 || dy /= 0 `blame` (dx, dy)) $
  let angle :: Double
      angle = atan (fromIntegral dy / fromIntegral dx) / (pi / 2)
      dxy | angle <= -0.75 = (0, -1)
          | angle <= -0.25 = (1, -1)
          | angle <= 0.25  = (1, 0)
          | angle <= 0.75  = (1, 1)
          | angle <= 1.25  = (0, 1)
          | otherwise = assert `failure` (lxsize, dx, dy, angle)
      rxy = if dx >= 0
            then VectorXY dxy
            else negXY $ VectorXY dxy
  in assert ((if isUnitXY v then v == rxy else True)
             `blame` (v, rxy))
     $ toDir lxsize rxy

-- TODO: Perhaps produce all acceptable directions and let AI choose.
-- That would also eliminate the Doubles. Or only directions from bla?
-- Smart monster could really use all dirs to be less predictable,
-- but it wouldn't look as natural as bla, so for less smart bla is better.
-- | Given two distinct positions, determine the direction (a unit vector)
-- in which one should move from the first in order to get closer
-- to the second. Ignores obstacles. Of several equally good directions
-- (in the chessboard metric) it picks one of those that visually
-- (in the euclidean metric) maximally align with the vector between
-- the two points..
towards :: X -> Point -> Point -> Vector
towards lxsize pos0 pos1 =
  assert (pos0 /= pos1 `blame` (pos0, pos1)) $
  let v = displacementXYZ lxsize pos0 pos1
  in normalize lxsize v

-- | A vector from a point to another. We have
--
-- > shift pos1 (displacement pos1 pos2) == pos2
--
-- Particularly simple and fast implementation in the linear representation.
displacement :: Point -> Point -> Vector
displacement pos1 pos2 = Vector $ fromEnum pos2 - fromEnum pos1

-- | A list of vectors between a list of points.
displacePath :: [Point] -> [Vector]
displacePath []  = []
displacePath lp1@(_ : lp2) =
  map (uncurry displacement) $ zip lp1 lp2

-- | A list of points that a list of vectors leads to.
shiftPath :: Point -> [Vector] -> [Point]
shiftPath _     [] = []
shiftPath start (v : vs) =
  let next = shift start v
  in next : shiftPath next vs
