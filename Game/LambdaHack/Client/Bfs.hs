{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-- | Breadth first search algorithms.
module Game.LambdaHack.Client.Bfs
  ( BfsDistance, MoveLegal(..), minKnownBfs, apartBfs
  , fillBfs, findPathBfs, accessBfs
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Bits (Bits, complement, (.&.), (.|.))

import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Vector

-- | Weighted distance between points along shortest paths.
newtype BfsDistance = BfsDistance Word8
  deriving (Show, Eq, Ord, Enum, Bounded, Bits)

-- | State of legality of moves between adjacent points.
data MoveLegal = MoveBlocked | MoveToOpen | MoveToClosed | MoveToUnknown
  deriving Eq

-- | The minimal distance value assigned to paths that don't enter
-- any unknown tiles.
minKnownBfs :: BfsDistance
minKnownBfs = toEnum $ (1 + fromEnum (maxBound :: BfsDistance)) `div` 2

-- | The distance value that denotes no legal path between points,
-- either due to blocked tiles or pathfinding aborted at earlier tiles.
apartBfs :: BfsDistance
apartBfs = pred minKnownBfs

-- | The distance value that denotes that path search was aborted
-- at this tile due to too large actual distance
-- and that the tile was known and not blocked.
-- It is also a true distance value for this tile
-- (shifted by minKnownBfs, as all distances of known tiles).
abortedKnownBfs :: BfsDistance
abortedKnownBfs = pred maxBound

-- | The distance value that denotes that path search was aborted
-- at this tile due to too large actual distance
-- and that the tile was unknown.
-- It is also a true distance value for this tile.
abortedUnknownBfs :: BfsDistance
abortedUnknownBfs = pred apartBfs

-- TODO: costly; use a ring buffer instead of the lists, don't call so often
-- | Fill out the given BFS array.
-- Unsafe @PointArray@ operations are OK here, because the intermediate
-- values of the vector don't leak anywhere outside nor are kept unevaluated
-- and so they can't be overwritten by the unsafe side-effect.
fillBfs :: (Point -> Point -> MoveLegal)  -- ^ is a move from known tile legal
        -> Point                          -- ^ starting position
        -> PointArray.Array BfsDistance   -- ^ initial array, with @apartBfs@
        -> PointArray.Array BfsDistance   -- ^ array with calculated distances
{-# INLINE fillBfs #-}
fillBfs isEnterable source aInitial =
  let bfs :: BfsDistance
          -> [Point]
          -> PointArray.Array BfsDistance
          -> PointArray.Array BfsDistance
      bfs distance predK a =
        let distCompl = distance .&. complement minKnownBfs
            processKnown (succK2, a2) pos =
              let fKnown (lK, lU) move =
                    let p = shift pos move
                        freshMv = a2 PointArray.! p == apartBfs
                        legality = isEnterable pos p
                        (notBlocked, enteredUnknown) = case legality of
                          MoveBlocked -> (False, assert `failure` ())
                          MoveToOpen -> (True, False)
                          MoveToClosed -> (True, False)
                          MoveToUnknown -> (True, True)
                    in if freshMv && notBlocked
                       then if enteredUnknown
                            then (lK, p : lU)
                            else (p : lK, lU)
                       else (lK, lU)
                  (mvsK, mvsU) = foldl' fKnown ([], []) moves
                  upd = zip mvsK (repeat distance)
                        ++ zip mvsU (repeat distCompl)
                  !a3 = PointArray.unsafeUpdateA a2 upd
              in (mvsK ++ succK2, a3)
            (succK4, !a4) = foldl' processKnown ([], a) predK
        in if null succK4 || distance == abortedKnownBfs
           then a4  -- no more dungeon positions to check or too far
           else bfs (succ distance) succK4 a4
  in bfs (succ minKnownBfs) [source]
         (PointArray.unsafeUpdateA aInitial [(source, minKnownBfs)])

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
            -> Point -> Point -> Int
            -> PointArray.Array BfsDistance
            -> Maybe [Point]
{-# INLINE findPathBfs #-}
findPathBfs isEnterable source target sepsRaw bfs =
  assert (bfs PointArray.! source == minKnownBfs) $
  let eps = sepsRaw `mod` 4
      (mc1, mc2) = splitAt eps movesCardinal
      (md1, md2) = splitAt eps movesDiagonal
      preferredMoves = mc1 ++ reverse mc2 ++ md2 ++ reverse md1  -- fuzz
      track :: Point -> BfsDistance -> [Point] -> [Point]
      track pos oldDist suffix | oldDist == minKnownBfs =
        assert (pos == source
                `blame` (source, target, pos, suffix)) suffix
      track pos oldDist suffix =
        let dist = pred oldDist
            children = map (shift pos) preferredMoves
            f acc@(lo, lc) p = if bfs PointArray.! p /= dist
                               then acc
                               else case isEnterable p pos of
                                 MoveToOpen -> (p : lo, lc)
                                 MoveToUnknown -> (p : lo, lc)
                                 MoveToClosed -> (lo, p : lc)
                                 MoveBlocked -> acc
            (childrenOpen, childrenClosed) = foldl' f ([], []) children
            -- Prefer paths through open or unknown tiles.
            minP = case childrenOpen ++ childrenClosed of
              p : _ -> p
              [] -> assert `failure` (pos, oldDist, children)
        in track minP dist (pos : suffix)
      targetDist = bfs PointArray.! target
  in if targetDist /= apartBfs
     then Just $ track target (targetDist .|. minKnownBfs) []
     else let f :: (Point, BfsDistance, Int) -> Point -> BfsDistance
                -> (Point, BfsDistance, Int)
              f acc@(pAcc, dAcc, chessAcc) p d =
                if d > abortedUnknownBfs && d /= abortedKnownBfs
                then acc
                else let chessNew = chessDist p target
                     in case compare chessNew chessAcc of
                       LT -> (p, d, chessNew)
                       EQ -> case compare d dAcc of
                         LT -> (p, d, chessNew)
                         EQ | euclidDistSq p target
                              < euclidDistSq pAcc target -> (p, d, chessNew)
                         _ -> acc
                       _ -> acc
              (pRes, dRes, chessRes) =
                PointArray.ifoldlA' f (originPoint, apartBfs, maxBound) bfs
          in if chessRes == maxBound
             then Nothing
             else Just $ track pRes (dRes .|. minKnownBfs) []

-- | Access a BFS array and interpret the looked up distance value.
accessBfs :: PointArray.Array BfsDistance -> Point -> Maybe Int
{-# INLINE accessBfs #-}
accessBfs bfs target =
  let dist = bfs PointArray.! target
  in if dist == apartBfs
     then Nothing
     else Just $ fromEnum $ dist .&. complement minKnownBfs
