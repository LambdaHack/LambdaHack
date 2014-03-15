{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic operations on 2D vectors represented in an efficient,
-- but not unique, way.
module Game.LambdaHack.Common.Vector
  ( Vector(..), isUnit, isDiagonal, neg, chessDistVector, euclidDistSqVector
  , moves, compassText, vicinity, vicinityCardinal
  , shift, shiftBounded, trajectoryToPath, displacement, pathToTrajectory
  , RadianAngle, rotate, towards
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Int (Int32)
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Common.Point

-- | 2D vectors in cartesian representation. Coordinates grow to the right
-- and down, so that the (1, 1) vector points to the bottom-right corner
-- of the screen.
data Vector = Vector
  { vx :: !X
  , vy :: !Y
  }
  deriving (Eq, Ord, Show, Read)

instance Binary Vector where
  put = put . (fromIntegral :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegral :: Int32 -> Int)) get

instance Enum Vector where
  fromEnum = fromEnumVector
  toEnum = toEnumVector

-- | Maximal supported vector X and Y coordinates.
maxVectorDim :: Int
{-# INLINE maxVectorDim #-}
maxVectorDim = 2 ^ (maxLevelDimExponent - 1) - 1

fromEnumVector :: Vector -> Int
{-# INLINE fromEnumVector #-}
fromEnumVector (Vector vx vy) = vx + vy * (2 ^ maxLevelDimExponent)

toEnumVector :: Int -> Vector
{-# INLINE toEnumVector #-}
toEnumVector n =
  let (y, x) = n `quotRem` (2 ^ maxLevelDimExponent)
      (vx, vy) = if x > maxVectorDim
                 then (x - 2 ^ maxLevelDimExponent, y + 1)
                 else if x < - maxVectorDim
                      then (x + 2 ^ maxLevelDimExponent, y - 1)
                      else (x, y)
  in Vector{..}

-- | Tells if a vector has length 1 in the chessboard metric.
isUnit :: Vector -> Bool
{-# INLINE isUnit #-}
isUnit v = chessDistVector v == 1

-- | Checks whether a unit vector is a diagonal direction,
-- as opposed to cardinal. If the vector is not unit,
-- it checks that the vector is not horizontal nor vertical.
isDiagonal :: Vector -> Bool
{-# INLINE isDiagonal #-}
isDiagonal (Vector x y) = x * y /= 0

-- | Reverse an arbirary vector.
neg :: Vector -> Vector
{-# INLINE neg #-}
neg (Vector vx vy) = Vector (-vx) (-vy)

-- | Squared euclidean distance between two vectors.
euclidDistSqVector :: Vector -> Vector -> Int
{-# INLINE euclidDistSqVector #-}
euclidDistSqVector (Vector x0 y0) (Vector x1 y1) =
  let square n = n ^ (2 :: Int)
  in square (x1 - x0) + square (y1 - y0)

-- | The lenght of a vector in the chessboard metric,
-- where diagonal moves cost 1.
chessDistVector :: Vector -> Int
{-# INLINE chessDistVector #-}
chessDistVector (Vector x y) = max (abs x) (abs y)

-- | Vectors of all unit moves in the chessboard metric,
-- clockwise, starting north-west.
moves :: [Vector]
moves =
  map (uncurry Vector)
    [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

moveTexts :: [Text]
moveTexts = ["NW", "N", "NE", "E", "SE", "S", "SW", "W"]

compassText :: Vector -> Text
compassText v = let m = EM.fromList $ zip moves moveTexts
                in fromMaybe (assert `failure` "not a unit vector"
                                     `twith` v) $ EM.lookup v m

-- | Vectors of all cardinal direction unit moves, clockwise, starting north.
movesCardinal :: [Vector]
movesCardinal = map (uncurry Vector) [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | All (8 at most) closest neighbours of a point within an area.
vicinity :: X -> Y   -- ^ limit the search to this area
         -> Point    -- ^ position to find neighbours of
         -> [Point]
vicinity lxsize lysize p =
  [ res | dxy <- moves
        , let res = shift p dxy
        , inside res (0, 0, lxsize - 1, lysize - 1) ]

-- | All (4 at most) cardinal direction neighbours of a point within an area.
vicinityCardinal :: X -> Y   -- ^ limit the search to this area
                 -> Point    -- ^ position to find neighbours of
                 -> [Point]
vicinityCardinal lxsize lysize p =
  [ res | dxy <- movesCardinal
        , let res = shift p dxy
        , inside res (0, 0, lxsize - 1, lysize - 1) ]

-- | Translate a point by a vector.
shift :: Point -> Vector -> Point
{-# INLINE shift #-}
shift (Point x0 y0) (Vector x1 y1) = Point (x0 + x1) (y0 + y1)

-- | Translate a point by a vector, but only if the result fits in an area.
shiftBounded :: X -> Y -> Point -> Vector -> Point
shiftBounded lxsize lysize pos v@(Vector xv yv) =
  if inside pos (-xv, -yv, lxsize - xv - 1, lysize - yv - 1)
  then shift pos v
  else pos

-- | A list of points that a list of vectors leads to.
trajectoryToPath :: Point -> [Vector] -> [Point]
trajectoryToPath _ [] = []
trajectoryToPath start (v : vs) = let next = shift start v
                           in next : trajectoryToPath next vs

-- | A vector from a point to another. We have
--
-- > shift pos1 (displacement pos1 pos2) == pos2
displacement :: Point -> Point -> Vector
{-# INLINE displacement #-}
displacement (Point x0 y0) (Point x1 y1) = Vector (x1 - x0) (y1 - y0)

-- | A list of vectors between a list of points.
pathToTrajectory :: [Point] -> [Vector]
pathToTrajectory [] = []
pathToTrajectory lp1@(_ : lp2) = zipWith displacement lp1 lp2

type RadianAngle = Double

-- | Rotate a vector by the given angle (expressed in radians)
-- counterclockwise and return a unit vector approximately in the resulting
-- direction.
rotate :: RadianAngle -> Vector -> Vector
rotate angle (Vector x' y') =
  let x = fromIntegral x'
      y = fromIntegral y'
      -- Minus before the angle comes from our coordinates being
      -- mirrored along the X axis (Y coordinates grow going downwards).
      dx = x * cos (-angle) - y * sin (-angle)
      dy = x * sin (-angle) + y * cos (-angle)
  in normalize dx dy

-- TODO: use bla for that
-- | Given a vector of arbitrary non-zero length, produce a unit vector
-- that points in the same direction (in the chessboard metric).
-- Of several equally good directions it picks one of those that visually
-- (in the euclidean metric) maximally align with the original vector.
normalize :: Double -> Double -> Vector
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
     then uncurry Vector dxy
     else neg $ uncurry Vector dxy

normalizeVector :: Vector -> Vector
normalizeVector v@(Vector vx vy) =
  let res = normalize (fromIntegral vx) (fromIntegral vy)
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
