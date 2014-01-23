{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic operations on 2D vectors represented in an efficient,
-- but not unique, way.
module Game.LambdaHack.Common.Vector
  ( Vector(..), isUnit, isDiagonal, neg, euclidDistSq
  , moves, vicinity, vicinityCardinal
  , shift, shiftBounded, shiftPath, displacement, displacePath
  , RadianAngle, rotate, towards
  , BfsDistance, MoveLegal(..), minKnown
  , fillBfs, findPathBfs, accessBfs, posAimsPos
  ) where

import Control.Arrow (second)
import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits (Bits, complement, (.&.), (.|.))
import Data.Int (Int32)
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq

import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

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
euclidDistSq :: Vector -> Vector -> Int
{-# INLINE euclidDistSq #-}
euclidDistSq (Vector x0 y0) (Vector x1 y1) =
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
shiftPath :: Point -> [Vector] -> [Point]
shiftPath _ [] = []
shiftPath start (v : vs) = let next = shift start v
                           in next : shiftPath next vs

-- | A vector from a point to another. We have
--
-- > shift pos1 (displacement pos1 pos2) == pos2
displacement :: Point -> Point -> Vector
{-# INLINE displacement #-}
displacement (Point x0 y0) (Point x1 y1) = Vector (x1 - x0) (y1 - y0)

-- | A list of vectors between a list of points.
displacePath :: [Point] -> [Vector]
displacePath [] = []
displacePath lp1@(_ : lp2) = zipWith displacement lp1 lp2

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

newtype BfsDistance = BfsDistance Word8
  deriving (Show, Eq, Ord, Enum, Bounded, Bits)

data MoveLegal = MoveBlocked | MoveToOpen | MoveToUnknown
  deriving Eq

minKnown :: BfsDistance
minKnown = toEnum $ (1 + fromEnum (maxBound :: BfsDistance)) `div` 2

-- TODO: Move somewhere; in particular, only clients need to know that.
fillBfs :: (Point -> Point -> MoveLegal)  -- ^ is move from a known tile legal
        -> (Point -> Point -> Bool)       -- ^ is a move from unknown legal
        -> Point                          -- ^ starting position
        -> PointArray.Array BfsDistance   -- ^ initial array, with @maxBound@
        -> PointArray.Array BfsDistance   -- ^ array with calculated distances
fillBfs isEnterable passUnknown origin aInitial =
  -- TODO: copy, thaw, mutate, freeze
  let maxUnknown = pred minKnown
      bfs :: Seq.Seq (Point, BfsDistance)
          -> PointArray.Array BfsDistance
          -> PointArray.Array BfsDistance
      bfs q a =
        case Seq.viewr q of
          Seq.EmptyR -> a  -- no more positions to check
          _ Seq.:> (_, d)| d == maxUnknown || d == maxBound -> a  -- too far
          q1 Seq.:> (pos, oldDistance) | oldDistance >= minKnown ->
            let distance = succ oldDistance
                allMvs = map (shift pos) moves
                freshMv p = a PointArray.! p == maxBound
                freshMvs = filter freshMv allMvs
                legal p = (p, isEnterable pos p)
                legalities = map legal freshMvs
                notBlocked = filter ((/= MoveBlocked) . snd) legalities
                legalToDist l = if l == MoveToOpen
                                then distance
                                else distance .&. complement minKnown
                mvs = map (second legalToDist) notBlocked
                q2 = foldr (Seq.<|) q1 mvs
                s2 = a PointArray.// mvs
            in bfs q2 s2
          q1 Seq.:> (pos, oldDistance) ->
            let distance = succ oldDistance
                allMvs = map (shift pos) moves
                goodMv p = a PointArray.! p == maxBound && passUnknown pos p
                mvs = zip (filter goodMv allMvs) (repeat distance)
                q2 = foldr (Seq.<|) q1 mvs
                s2 = a PointArray.// mvs
            in bfs q2 s2
      origin0 = (origin, minKnown)
  in bfs (Seq.singleton origin0) (aInitial PointArray.// [origin0])

-- TODO: Use http://harablog.wordpress.com/2011/09/07/jump-point-search/
-- to determine a few really different paths and compare them,
-- e.g., how many closed doors they pass, open doors, unknown tiles
-- on the path or close enough to reveal them.
-- Also, check if JPS can somehow optimize BFS or pathBfs.
-- | Find a path with the smallest length.
-- The @eps@ coefficient determines which direction (or the closest
-- directions available) that path should prefer, where 0 means north-west
-- and 1 means north.
findPathBfs :: Point -> Point -> Int -> (PointArray.Array BfsDistance)
            -> Maybe [Point]
findPathBfs source target sepsRaw bfs =
  assert (bfs PointArray.! source == minKnown) $
  let targetDist = bfs PointArray.! target
  in if targetDist == maxBound
     then Nothing
     else
       let eps = abs sepsRaw `mod` length moves
           mix (x : xs) ys = x : mix ys xs
           mix [] ys = ys
           preferedMoves = let (ch1, ch2) = splitAt eps moves
                               ch = ch2 ++ ch1
                           in mix ch (reverse ch)
           track :: Point -> BfsDistance -> [Point] -> [Point]
           track pos oldDist suffix | oldDist == minKnown =
             assert (pos == source) suffix
           track pos oldDist suffix | oldDist >= minKnown =
             let dist = pred oldDist
                 children = map (shift pos) preferedMoves
                 matchesDist p = bfs PointArray.! p == dist
                 minP = fromMaybe (assert `failure` (pos, oldDist, children))
                                  (find matchesDist children)
             in track minP dist (pos : suffix)
           track pos oldDist suffix =
             let distUnknown = pred oldDist
                 distKnown = distUnknown .|. minKnown
                 children = map (shift pos) preferedMoves
                 matchesDistUnknown p = bfs PointArray.! p == distUnknown
                 matchesDistKnown p = bfs PointArray.! p == distKnown
                 (minP, dist) = case find matchesDistKnown children of
                   Just p -> (p, distKnown)
                   Nothing -> case find matchesDistUnknown children of
                     Just p -> (p, distUnknown)
                     Nothing -> assert `failure` (pos, oldDist, children)
             in track minP dist (pos : suffix)
       in Just $ track target targetDist []

accessBfs :: PointArray.Array BfsDistance -> Point -> Maybe Int
{-# INLINE accessBfs #-}
accessBfs bfs target =
  let dist = bfs PointArray.! target
  in if dist == maxBound
     then Nothing
     else Just $ fromEnum $ dist .&. complement minKnown

posAimsPos :: PointArray.Array BfsDistance -> Point -> Point -> Bool
{-# INLINE posAimsPos #-}
posAimsPos bfs bpos target =
  let mdist = accessBfs bfs target
  in mdist == Just (chessDist bpos target)
