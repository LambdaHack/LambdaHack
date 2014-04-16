{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Breadth first search algorithms.
module Game.LambdaHack.Client.Bfs
  ( BfsDistance, MoveLegal(..), apartBfs
  , fillBfs, findPathBfs, accessBfs
  ) where

import Control.Arrow (second)
import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits (Bits, complement, (.&.), (.|.))
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq

import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Vector

newtype BfsDistance = BfsDistance Word8
  deriving (Show, Eq, Ord, Enum, Bounded, Bits)

data MoveLegal = MoveBlocked | MoveToOpen | MoveToUnknown
  deriving Eq

minKnownBfs :: BfsDistance
minKnownBfs = toEnum $ (1 + fromEnum (maxBound :: BfsDistance)) `div` 2

apartBfs :: BfsDistance
apartBfs = pred minKnownBfs

-- TODO: Move somewhere; in particular, only clients need to know that.
fillBfs :: (Point -> Point -> MoveLegal)  -- ^ is move from a known tile legal
        -> (Point -> Point -> Bool)       -- ^ is a move from unknown legal
        -> Point                          -- ^ starting position
        -> PointArray.Array BfsDistance   -- ^ initial array, with @apartBfs@
        -> PointArray.Array BfsDistance   -- ^ array with calculated distances
fillBfs isEnterable passUnknown origin aInitial =
  -- TODO: copy, thaw, mutate, freeze
  let maxUnknownBfs = pred apartBfs
      maxKnownBfs = pred maxBound
      bfs :: Seq.Seq (Point, BfsDistance)
          -> PointArray.Array BfsDistance
          -> PointArray.Array BfsDistance
      bfs q a =
        case Seq.viewr q of
          Seq.EmptyR -> a  -- no more positions to check
          _ Seq.:> (_, d)
            | d == maxUnknownBfs || d == maxKnownBfs -> a  -- too far
          q1 Seq.:> (pos, oldDistance) | oldDistance >= minKnownBfs ->
            let distance = succ oldDistance
                allMvs = map (shift pos) moves
                freshMv p = a PointArray.! p == apartBfs
                freshMvs = filter freshMv allMvs
                legal p = (p, isEnterable pos p)
                legalities = map legal freshMvs
                notBlocked = filter ((/= MoveBlocked) . snd) legalities
                legalToDist l = if l == MoveToOpen
                                then distance
                                else distance .&. complement minKnownBfs
                mvs = map (second legalToDist) notBlocked
                q2 = foldr (Seq.<|) q1 mvs
                s2 = a PointArray.// mvs
            in bfs q2 s2
          q1 Seq.:> (pos, oldDistance) ->
            let distance = succ oldDistance
                allMvs = map (shift pos) moves
                goodMv p = a PointArray.! p == apartBfs && passUnknown pos p
                mvs = zip (filter goodMv allMvs) (repeat distance)
                q2 = foldr (Seq.<|) q1 mvs
                s2 = a PointArray.// mvs
            in bfs q2 s2
      origin0 = (origin, minKnownBfs)
  in bfs (Seq.singleton origin0) (aInitial PointArray.// [origin0])

-- TODO: Use http://harablog.wordpress.com/2011/09/07/jump-point-search/
-- to determine a few really different paths and compare them,
-- e.g., how many closed doors they pass, open doors, unknown tiles
-- on the path or close enough to reveal them.
-- Also, check if JPS can somehow optimize BFS or pathBfs.
-- | Find a path, without the source position, with the smallest length.
-- The @eps@ coefficient determines which direction (or the closest
-- directions available) that path should prefer, where 0 means north-west
-- and 1 means north.
findPathBfs :: (Point -> Point -> MoveLegal)
            -> (Point -> Point -> Bool)
            -> Point -> Point -> Int -> PointArray.Array BfsDistance
            -> Maybe [Point]
findPathBfs isEnterable passUnknown source target sepsRaw bfs =
  assert (bfs PointArray.! source == minKnownBfs) $
  let targetDist = bfs PointArray.! target
  in if targetDist == apartBfs
     then Nothing
     else
       let eps = sepsRaw `mod` 4
           (mc1, mc2) = splitAt eps movesCardinal
           (md1, md2) = splitAt eps movesDiagonal
           preferredMoves = mc1 ++ reverse mc2 ++ md2 ++ reverse md1  -- fuzz
           track :: Point -> BfsDistance -> [Point] -> [Point]
           track pos oldDist suffix | oldDist == minKnownBfs =
             assert (pos == source
                     `blame` (source, target, pos, suffix)) suffix
           track pos oldDist suffix | oldDist > minKnownBfs =
             let dist = pred oldDist
                 children = map (shift pos) preferredMoves
                 matchesDist p = bfs PointArray.! p == dist
                                 && isEnterable p pos == MoveToOpen
                 minP = fromMaybe (assert `failure` (pos, oldDist, children))
                                  (find matchesDist children)
             in track minP dist (pos : suffix)
           track pos oldDist suffix =
             let distUnknown = pred oldDist
                 distKnown = distUnknown .|. minKnownBfs
                 children = map (shift pos) preferredMoves
                 matchesDistUnknown p = bfs PointArray.! p == distUnknown
                                        && passUnknown p pos
                 matchesDistKnown p = bfs PointArray.! p == distKnown
                                      && isEnterable p pos == MoveToUnknown
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
  in if dist == apartBfs
     then Nothing
     else Just $ fromEnum $ dist .&. complement minKnownBfs
