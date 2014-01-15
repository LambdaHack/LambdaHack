{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic operations on 2D vectors represented in an efficient,
-- but not unique, way.
module Game.LambdaHack.Common.Vector
  ( Vector, toVector, toDir, shift, shiftBounded, moves
  , isUnit, euclidDistSq, diagonal
  , neg, RadianAngle, rotate
  , towards, displacement, displacePath, shiftPath
  , vicinity, vicinityCardinal, vicinityCardinalXY
  , BfsDistance, MoveLegal(..), bfsFill
  ) where

import Control.Arrow (second)
import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits (Bits, unsafeShiftL, (.|.))
import Data.Int (Int32)
import qualified Data.Sequence as Seq

import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.VectorXY

-- | 2D vectors  represented as offsets in the linear framebuffer
-- indexed by 'Point'.
-- A newtype is used to prevent mixing up the type with @Point@ itself.
newtype Vector = Vector Int
  deriving (Eq, Ord, Enum, Read)

instance Binary Vector where
  put = put . (fromIntegral :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegral :: Int32 -> Int)) get

-- For debugging.
instance Show Vector where
  show = show . fromVector

-- | Converts a vector in cartesian representation into @Vector@.
toVector :: VectorXY -> Vector
{-# INLINE toVector #-}
toVector =
  let fromEnumXY (VectorXY x y) = x + unsafeShiftL y maxLevelDimExponent
  in toEnum . fromEnumXY

-- | Converts a unit vector in cartesian representation into @Vector@.
toDir :: VectorXY -> Vector
{-# INLINE toDir #-}
toDir vxy =
  assert (isUnitXY vxy `blame` "not a unit VectorXY" `twith` vxy)
  $ toVector vxy

isUnitXY :: VectorXY -> Bool
{-# INLINE isUnitXY #-}
isUnitXY vxy = chessDistXY vxy == 1

-- | Tells if a vector has length 1 in the chessboard metric.
isUnit ::  Vector -> Bool
{-# INLINE isUnit #-}
isUnit = isUnitXY . fromVector

-- | Converts a vector in the offset representation
-- into the cartesian representation.
fromVector :: Vector -> VectorXY
{-# INLINE fromVector #-}
fromVector =
  let toEnumXY xy =
        let (y, x) = xy `quotRem` (2 ^ maxLevelDimExponent)
            (vx, vy) = if x > maxVectorDim
                       then (x - 2 ^ maxLevelDimExponent, y + 1)
                       else if x < - maxVectorDim
                            then (x + 2 ^ maxLevelDimExponent, y - 1)
                            else (x, y)
        in VectorXY{..}
  in toEnumXY . fromEnum

-- | Converts a unit vector in the offset representation
-- into the cartesian representation.
fromDir :: Vector -> VectorXY
{-# INLINE fromDir #-}
fromDir v =
  assert (isUnit v `blame` "not a unit Vector" `twith` v)
  $ fromVector v

-- | Translate a point by a vector.
--
-- Particularly simple and fast implementation in the linear representation.
shift :: Point -> Vector -> Point
{-# INLINE shift #-}
shift p (Vector dir) = toEnum $ fromEnum p + dir

-- | Translate a point by a vector, but only if the result fits in an area.
shiftBounded :: X -> Y -> Point -> Vector -> Point
shiftBounded lxsize lysize pos dir =
  let VectorXY xv yv = fromVector dir
  in if inside pos (-xv, -yv, lxsize - xv - 1, lysize - yv - 1)
     then shift pos dir
     else pos

-- | Vectors of all unit moves in the chessboard metric,
-- clockwise, starting north-west.
moves :: [Vector]
moves = map toDir movesXY

-- | Squared euclidean distance between two unit vectors.
euclidDistSq :: Vector -> Vector -> Int
{-# INLINE euclidDistSq #-}
euclidDistSq dir0 dir1
  | VectorXY x0 y0 <- fromDir dir0
  , VectorXY x1 y1 <- fromDir dir1 =
  euclidDistSqXY $ VectorXY (x1 - x0) (y1 - y0)

-- | Checks whether a unit vector is a diagonal direction,
-- as opposed to cardinal. If the vector is not unit,
-- it checks that the vector is not horizontal nor vertical.
diagonal :: Vector -> Bool
{-# INLINE diagonal #-}
diagonal dir | VectorXY x y <- fromDir dir =
  x * y /= 0

-- | Reverse an arbirary vector.
neg :: Vector -> Vector
{-# INLINE neg #-}
neg (Vector dir) = Vector (-dir)

type RadianAngle = Double

-- | Rotate a vector by the given angle (expressed in radians)
-- counterclockwise and return a unit vector approximately in the resulting
-- direction.
rotate :: RadianAngle -> Vector -> Vector
rotate angle dir | VectorXY x' y' <- fromDir dir =
  let x = fromIntegral x'
      y = fromIntegral y'
      -- Minus before the angle comes from our coordinates being
      -- mirrored along the X axis (Y coordinates grow going downwards).
      dx = x * cos (-angle) - y * sin (-angle)
      dy = x * sin (-angle) + y * cos (-angle)
      rxy = normalize dx dy
  in toDir rxy

-- TODO: use bla for that
-- | Given a vector of arbitrary non-zero length, produce a unit vector
-- that points in the same direction (in the chessboard metric).
-- Of several equally good directions it picks one of those that visually
-- (in the euclidean metric) maximally align with the original vector.
normalize :: Double -> Double -> VectorXY
normalize dx dy =
  assert (dx /= 0 || dy /= 0 `blame` "can't normalize zero" `twith` (dx, dy)) $
  let angle :: Double
      angle = atan (dy / dx) / (pi / 2)
      dxy | angle <= -0.75 && angle >= -1.25 = (0, -1)
          | angle <= -0.25 = (1, -1)
          | angle <= 0.25  = (1, 0)
          | angle <= 0.75  = (1, 1)
          | angle <= 1.25  = (0, 1)
          | otherwise = assert `failure` "impossible angle"
                               `twith` (dx, dy, angle)
  in if dx >= 0
     then uncurry VectorXY dxy
     else negXY $ uncurry VectorXY dxy

normalizeVector :: Vector -> Vector
normalizeVector v =
  let VectorXY dx dy = fromVector v
      res = toDir $ normalize (fromIntegral dx) (fromIntegral dy)
  in assert (not (isUnit v) || v == res
             `blame` "unit vector gets untrivially normalized"
             `twith` (v, res))
     res

-- TODO: Perhaps produce all acceptable directions and let AI choose.
-- That would also eliminate the Doubles. Or only directions from bla?
-- Smart monster could really use all dirs to be less predictable,
-- but it wouldn't look as natural as bla, so for less smart bla is better.
-- | Given two distinct positions, determine the direction (a unit vector)
-- in which one should move from the first in order to get closer
-- to the second. Ignores obstacles. Of several equally good directions
-- (in the chessboard metric) it picks one of those that visually
-- (in the euclidean metric) maximally align with the vector between
-- the two points.
towards :: Point -> Point -> Vector
towards pos0 pos1 =
  assert (pos0 /= pos1 `blame` "towards self" `twith` (pos0, pos1))
  $ normalizeVector $ displacement pos0 pos1

-- | A vector from a point to another. We have
--
-- > shift pos1 (displacement pos1 pos2) == pos2
--
-- Particularly simple and fast implementation in the linear representation.
displacement :: Point -> Point -> Vector
{-# INLINE displacement #-}
displacement pos1 pos2 = Vector $ fromEnum pos2 - fromEnum pos1

-- | A list of vectors between a list of points.
displacePath :: [Point] -> [Vector]
displacePath []  = []
displacePath lp1@(_ : lp2) = zipWith displacement lp1 lp2

-- | A list of points that a list of vectors leads to.
shiftPath :: Point -> [Vector] -> [Point]
shiftPath _     [] = []
shiftPath start (v : vs) =
  let next = shift start v
  in next : shiftPath next vs

-- | Returns the 8, or less, surrounding positions of a given position.
vicinity :: X -> Y -> Point -> [Point]
{-# INLINE vicinity #-}
vicinity lxsize lysize p =
  map toPoint $
    vicinityXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint p

-- | Returns the 4, or less, surrounding positions in cardinal directions
-- from a given position.
vicinityCardinal :: X -> Y -> Point -> [Point]
{-# INLINE vicinityCardinal #-}
vicinityCardinal lxsize lysize p =
  map toPoint $
    vicinityCardinalXY (0, 0, lxsize - 1, lysize - 1) $
      fromPoint p

-- | Checks that a point belongs to an area.
insideXY :: PointXY -> (X, Y, X, Y) -> Bool
{-# INLINE insideXY #-}
insideXY (PointXY x y) (x0, y0, x1, y1) =
  x1 >= x && x >= x0 && y1 >= y && y >= y0

-- | Shift a point by a vector.
shiftXY :: PointXY -> VectorXY -> PointXY
{-# INLINE shiftXY #-}
shiftXY (PointXY x0 y0) (VectorXY x1 y1) = PointXY (x0 + x1) (y0 + y1)

-- | Vectors of all unit moves in the chessboard metric,
-- clockwise, starting north-west.
movesXY :: [VectorXY]
movesXY =
  map (uncurry VectorXY)
    [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- | Vectors of all cardinal direction unit moves, clockwise, starting north.
movesCardinalXY :: [VectorXY]
movesCardinalXY = map (uncurry VectorXY) [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | The lenght of a vector in the chessboard metric,
-- where diagonal moves cost 1.
chessDistXY :: VectorXY -> Int
{-# INLINE chessDistXY #-}
chessDistXY (VectorXY x y) = max (abs x) (abs y)

-- | Squared euclidean length of a vector.
euclidDistSqXY :: VectorXY -> Int
{-# INLINE euclidDistSqXY #-}
euclidDistSqXY (VectorXY x y) = x * x + y * y

-- | Reverse an arbirary vector.
negXY :: VectorXY -> VectorXY
{-# INLINE negXY #-}
negXY (VectorXY x y) = VectorXY (-x) (-y)

-- | All (8 at most) closest neighbours of a point within an area.
vicinityXY :: (X, Y, X, Y)  -- ^ limit the search to this area
           -> PointXY       -- ^ position to find neighbours of
           -> [PointXY]
vicinityXY area xy =
  [ res | dxy <- movesXY, let res = shiftXY xy dxy, insideXY res area ]

-- | All (4 at most) cardinal direction neighbours of a point within an area.
vicinityCardinalXY :: (X, Y, X, Y)  -- ^ limit the search to this area
                   -> PointXY       -- ^ position to find neighbours of
                   -> [PointXY]
vicinityCardinalXY area xy =
  [ res | dxy <- movesCardinalXY
        , let res = shiftXY xy dxy
        , insideXY res area ]

newtype BfsDistance = BfsDistance Word8
  deriving (Show, Eq, Ord, Enum, Bounded, Bits)

data MoveLegal = MoveBlocked | MoveToOpen | MoveToUnknown
  deriving Eq

bfsFill :: (Point -> Point -> MoveLegal)  -- ^ is move from a known tile legal
        -> (Point -> Point -> Bool)       -- ^ is a move from unknown legal
        -> Point                          -- ^ starting position
        -> PointArray.Array BfsDistance   -- ^ initial array, with @maxBound@
        -> PointArray.Array BfsDistance   -- ^ array with calculated distances
bfsFill isEnterable passUnknown origin aInitial =
  -- TODO: copy, thaw, mutate, freeze
  let unknownBound = toEnum $ (1 + fromEnum (maxBound :: BfsDistance)) `div` 2
      bfs :: Seq.Seq (Point, BfsDistance)
          -> PointArray.Array BfsDistance
          -> PointArray.Array BfsDistance
      bfs q a =
        case Seq.viewr q of
          Seq.EmptyR -> a  -- no more positions to check
          _ Seq.:> (_, d)| d == unknownBound || d == maxBound -> a  -- too far
          q1 Seq.:> (pos, oldDistance) | oldDistance > unknownBound ->
            let distance = toEnum $ fromEnum oldDistance + 1
                allMvs = map (shift pos) moves
                goodMv p = a PointArray.! p == maxBound && passUnknown pos p
                mvs = zip (filter goodMv allMvs) (repeat distance)
                q2 = foldr (Seq.<|) q1 mvs
                s2 = a PointArray.// mvs
            in bfs q2 s2
          q1 Seq.:> (pos, oldDistance) ->
            let distance = toEnum $ fromEnum oldDistance + 1
                allMvs = map (shift pos) moves
                freshMv p = a PointArray.! p == maxBound
                freshMvs = filter freshMv allMvs
                legal p = (p, isEnterable pos p)
                legalities = map legal freshMvs
                notBlocked = filter ((/= MoveBlocked) . snd) legalities
                legalToDist l = if l == MoveToOpen
                                then distance
                                else distance .|. unknownBound
                mvs = map (second legalToDist) notBlocked
                q2 = foldr (Seq.<|) q1 mvs
                s2 = a PointArray.// mvs
            in bfs q2 s2
      origin0 = (origin, toEnum 0)
  in bfs (Seq.singleton origin0) (aInitial PointArray.// [origin0])
