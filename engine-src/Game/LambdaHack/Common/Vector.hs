{-# LANGUAGE DeriveGeneric #-}
-- | Basic operations on bounded 2D vectors, with an efficient, but not 1-1
-- and not monotonic @Enum@ instance.
module Game.LambdaHack.Common.Vector
  ( Vector(..), VectorI
  , isUnit, neg, chessDistVector, euclidDistSqVector
  , moves, movesCardinal, movesCardinalI, movesDiagonal, movesDiagonalI
  , compassText, vicinityBounded, vicinityUnsafe
  , vicinityCardinal, vicinityCardinalUnsafe, squareUnsafeSet
  , shift, shiftBounded, trajectoryToPath, trajectoryToPathBounded
  , vectorToFrom, computeTrajectory
  , RadianAngle, rotate, towards
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , longMoveTexts, movesSquare, pathToTrajectory
  , normalize, normalizeVector
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int32)
import qualified Data.IntSet as IS
import qualified Data.Primitive.PrimArray as PA
import           GHC.Generics (Generic)

import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Definition.Defs

-- | 2D vectors in cartesian representation. Coordinates grow to the right
-- and down, so that the (1, 1) vector points to the bottom-right corner
-- of the screen.
data Vector = Vector
  { vx :: X
  , vy :: Y
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary Vector where
  put = put . (toIntegralCrash :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegralWrap :: Int32 -> Int)) get
    -- `fromIntegralWrap` is fine here, because we converted the integer
    -- in the opposite direction first, so it fits even in 31 bit `Int`

-- Note that the conversion is not monotonic wrt the natural @Ord@ instance,
-- to keep it in sync with Point.
instance Enum Vector where
  fromEnum Vector{..} =
    let !xsize = PA.indexPrimArray speedupHackXSize 0
    in vx + vy * xsize
  toEnum n =
    let !xsize = PA.indexPrimArray speedupHackXSize 0
        !xsizeHalf = xsize `div` 2
        (!y, !x) = n `quotRem` xsize
        (!vx, !vy) | x >= xsizeHalf = (x - xsize, y + 1)
                   | x <= - xsizeHalf = (x + xsize, y - 1)
                   | otherwise = (x, y)
    in Vector{..}

instance NFData Vector

-- | Enumeration representation of @Vector@.
type VectorI = Int

-- | Tells if a vector has length 1 in the chessboard metric.
isUnit :: Vector -> Bool
{-# INLINE isUnit #-}
isUnit v = chessDistVector v == 1

-- | Reverse an arbirary vector.
neg :: Vector -> Vector
{-# INLINE neg #-}
neg (Vector vx vy) = Vector (-vx) (-vy)

-- | The lenght of a vector in the chessboard metric,
-- where diagonal moves cost 1.
chessDistVector :: Vector -> Int
{-# INLINE chessDistVector #-}
chessDistVector (Vector x y) = max (abs x) (abs y)

-- | Squared euclidean distance between two vectors.
euclidDistSqVector :: Vector -> Vector -> Int
euclidDistSqVector (Vector x0 y0) (Vector x1 y1) =
  (x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int)

-- | Vectors of all unit moves in the chessboard metric,
-- clockwise, starting north-west.
moves :: [Vector]
moves =
  map (uncurry Vector)
    [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- | Vectors of all cardinal direction unit moves, clockwise, starting north.
movesCardinal :: [Vector]
movesCardinal = map (uncurry Vector) [(0, -1), (1, 0), (0, 1), (-1, 0)]

movesCardinalI :: [VectorI]
movesCardinalI = map fromEnum movesCardinal

-- | Vectors of all diagonal direction unit moves, clockwise, starting north.
movesDiagonal :: [Vector]
movesDiagonal = map (uncurry Vector) [(-1, -1), (1, -1), (1, 1), (-1, 1)]

movesDiagonalI :: [VectorI]
movesDiagonalI = map fromEnum movesDiagonal

-- moveTexts :: [Text]
-- moveTexts = ["NW", "N", "NE", "E", "SE", "S", "SW", "W"]

longMoveTexts :: [Text]
longMoveTexts = [ "northwest", "north", "northeast", "east"
                , "southeast", "south", "southwest", "west" ]

compassText :: Vector -> Text
compassText v = let m = EM.fromList $ zip moves longMoveTexts
                    assFail = error $ "not a unit vector" `showFailure` v
                in EM.findWithDefault assFail v m

-- | Checks that a point belongs to an area.
insideP :: Point -> (X, Y, X, Y) -> Bool
{-# INLINE insideP #-}
insideP (Point x y) (x0, y0, x1, y1) = x1 >= x && x >= x0 && y1 >= y && y >= y0

-- | All (8 at most) closest neighbours of a point within an area.
vicinityBounded :: X -> Y   -- ^ limit the search to this area
                -> Point    -- ^ position to find neighbours of
                -> [Point]
vicinityBounded rWidthMax rHeightMax p =
  if insideP p (1, 1, rWidthMax - 2, rHeightMax - 2)
  then vicinityUnsafe p
  else [ res | dxy <- moves
             , let res = shift p dxy
             , insideP res (0, 0, rWidthMax - 1, rHeightMax - 1) ]

vicinityUnsafe :: Point -> [Point]
{-# INLINE vicinityUnsafe #-}
vicinityUnsafe p = [ shift p dxy | dxy <- moves ]

-- | All (4 at most) cardinal direction neighbours of a point within an area.
vicinityCardinal :: X -> Y   -- ^ limit the search to this area
                 -> Point    -- ^ position to find neighbours of
                 -> [Point]
vicinityCardinal rWidthMax rHeightMax p =
  [ res | dxy <- movesCardinal
        , let res = shift p dxy
        , insideP res (0, 0, rWidthMax - 1, rHeightMax - 1) ]

vicinityCardinalUnsafe :: Point -> [Point]
vicinityCardinalUnsafe p = [ shift p dxy | dxy <- movesCardinal ]

-- Ascending list; includes the origin.
movesSquare :: [VectorI]
movesSquare = map (fromEnum . uncurry Vector)
                  [ (-1, -1), (0, -1), (1, -1)
                  , (-1, 0), (0, 0), (1, 0)
                  , (-1, 1), (0, 1), (1, 1) ]

squareUnsafeSet :: Point -> ES.EnumSet Point
{-# INLINE squareUnsafeSet #-}
squareUnsafeSet p =
  ES.intSetToEnumSet $ IS.fromDistinctAscList $ map (fromEnum p +) movesSquare

-- | Translate a point by a vector.
shift :: Point -> Vector -> Point
{-# INLINE shift #-}
shift (Point x0 y0) (Vector x1 y1) = Point (x0 + x1) (y0 + y1)

-- | Translate a point by a vector, but only if the result fits in an area.
shiftBounded :: X -> Y -> Point -> Vector -> Point
shiftBounded rWidthMax rHeightMax pos v@(Vector xv yv) =
  if insideP pos (-xv, -yv, rWidthMax - xv - 1, rHeightMax - yv - 1)
  then shift pos v
  else pos

-- | A list of points that a list of vectors leads to.
trajectoryToPath :: Point -> [Vector] -> [Point]
trajectoryToPath _ [] = []
trajectoryToPath start (v : vs) = let next = shift start v
                                  in next : trajectoryToPath next vs

-- | A list of points that a list of vectors leads to, bounded by level size.
trajectoryToPathBounded :: X -> Y -> Point -> [Vector] -> [Point]
trajectoryToPathBounded _ _ _ [] = []
trajectoryToPathBounded rWidthMax rHeightMax start (v : vs) =
  let next = shiftBounded rWidthMax rHeightMax start v
  in next : trajectoryToPathBounded rWidthMax rHeightMax next vs

-- | The vector between the second point and the first. We have
--
-- > shift pos1 (pos2 `vectorToFrom` pos1) == pos2
--
-- The arguments are in the same order as in the underlying scalar subtraction.
vectorToFrom :: Point -> Point -> Vector
{-# INLINE vectorToFrom #-}
vectorToFrom (Point x0 y0) (Point x1 y1) = Vector (x0 - x1) (y0 - y1)

-- | A list of vectors between a list of points.
pathToTrajectory :: [Point] -> [Vector]
pathToTrajectory [] = []
pathToTrajectory lp1@(_ : lp2) = zipWith vectorToFrom lp2 lp1

computeTrajectory :: Int -> Int -> Int -> [Point] -> ([Vector], (Speed, Int))
computeTrajectory weight throwVelocity throwLinger path =
  let speed = speedFromWeight weight throwVelocity
      trange = rangeFromSpeedAndLinger speed throwLinger
      btrajectory = pathToTrajectory $ take (trange + 1) path
  in (btrajectory, (speed, trange))

type RadianAngle = Double

-- | Rotate a vector by the given angle (expressed in radians)
-- counterclockwise and return a unit vector approximately in the resulting
-- direction.
rotate :: RadianAngle -> Vector -> Vector
rotate angle (Vector x' y') =
  let x = intToDouble x'
      y = intToDouble y'
      -- Minus before the angle comes from our coordinates being
      -- mirrored along the X axis (Y coordinates grow going downwards).
      dx = x * cos (-angle) - y * sin (-angle)
      dy = x * sin (-angle) + y * cos (-angle)
  in normalize dx dy

-- | Given a vector of arbitrary non-zero length, produce a unit vector
-- that points in the same direction (in the chessboard metric).
-- Of several equally good directions it picks one of those that visually
-- (in the euclidean metric) maximally align with the original vector.
normalize :: Double -> Double -> Vector
normalize dx dy =
  assert (dx /= 0 || dy /= 0 `blame` "can't normalize zero" `swith` (dx, dy)) $
  let angle :: Double
      angle = atan (dy / dx) / (pi / 2)
      dxy | angle <= -0.75 && angle >= -1.25 = (0, -1)
          | angle <= -0.25 = (1, -1)
          | angle <= 0.25  = (1, 0)
          | angle <= 0.75  = (1, 1)
          | angle <= 1.25  = (0, 1)
          | otherwise = error $ "impossible angle" `showFailure` (dx, dy, angle)
  in if dx >= 0
     then uncurry Vector dxy
     else neg $ uncurry Vector dxy

normalizeVector :: Vector -> Vector
normalizeVector v@(Vector vx vy) =
  let res = normalize (intToDouble vx) (intToDouble vy)
  in assert (not (isUnit v) || v == res
             `blame` "unit vector gets untrivially normalized"
             `swith` (v, res))
     res

-- | Given two distinct positions, determine the direction (a unit vector)
-- in which one should move from the first in order to get closer
-- to the second. Ignores obstacles. Of several equally good directions
-- (in the chessboard metric) it picks one of those that visually
-- (in the euclidean metric) maximally align with the vector between
-- the two points.
towards :: Point -> Point -> Vector
towards pos0 pos1 =
  assert (pos0 /= pos1 `blame` "towards self" `swith` (pos0, pos1))
  $ normalizeVector $ pos1 `vectorToFrom` pos0
