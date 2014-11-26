{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-- | Breadth first search algorithms.
module Game.LambdaHack.Client.Bfs
  ( -- * Public API
    BfsDistance, MoveLegal(..), apartBfs
  , fillBfs, findPathBfs, accessBfs
#ifdef EXPOSE_INTERNAL
  , minKnownBfs
#endif
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits (Bits, complement, (.&.), (.|.))
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as U

import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Vector

-- | Weighted distance between points along shortest paths.
newtype BfsDistance = BfsDistance Word8
  deriving (Show, Eq, Ord, Enum, Bounded, Bits)

-- | State of legality of moves between adjacent points.
data MoveLegal = MoveBlocked | MoveToOpen | MoveToUnknown
  deriving Eq

-- | The minimal distance value assigned to paths that don't enter
-- any unknown tiles.
minKnownBfs :: BfsDistance
minKnownBfs = toEnum $ (1 + fromEnum (maxBound :: BfsDistance)) `div` 2

-- | The distance value that denote no legal path between points.
apartBfs :: BfsDistance
apartBfs = pred minKnownBfs

-- TODO: costly; peephole optimize, optmize BFS, don't call so often
-- | Fill out the given BFS array.
fillBfs :: (Point -> Point -> MoveLegal)  -- ^ is a move from known tile legal
        -> (Point -> Point -> Bool)       -- ^ is a move from unknown legal
        -> Point                          -- ^ starting position
        -> PointArray.Array BfsDistance   -- ^ initial array, with @apartBfs@
        -> PointArray.Array BfsDistance   -- ^ array with calculated distances
{-# INLINE fillBfs #-}
fillBfs isEnterable passUnknown origin aInitial =
  let maxKnownBfs = pred maxBound
      predMaxKnownBfs = pred maxKnownBfs
      movesV = U.fromList
               $ map (\(Vector x y) -> PointArray.aindex aInitial $ Point x y)
               moves
      bfs :: BfsDistance
          -> U.Vector Int  -- Int32? alloc big vector and take slices?
          -> U.Vector Int
          -> PointArray.Array BfsDistance
          -> PointArray.Array BfsDistance
      bfs distance predK predU a =
        let distCompl = distance .&. complement minKnownBfs
            processKnown (succK2, succU2, a2) posInt =
              let pos = PointArray.aunindex a2 posInt
                  fKnown (lK, lU) moveInt =
                    let pInt = posInt + moveInt
                        p = PointArray.aunindex a2 pInt
                        freshMv = a2 `PointArray.atInt` pInt == apartBfs
                        legality = isEnterable pos p
                        (notBlocked, enteredUnknown) = case legality of
                          MoveBlocked -> (False, undefined)
                          MoveToOpen -> (True, False)
                          MoveToUnknown -> (True, True)
                    in if freshMv && notBlocked
                       then if enteredUnknown
                            then (lK, U.cons pInt lU)
                            else (U.cons pInt lK, lU)
                       else (lK, lU)
                  (mvsK, mvsU) = U.foldl' fKnown (U.empty, U.empty) movesV
                  a3 = PointArray.updateConst a2 mvsK distance
                  a4 = PointArray.updateConst a3 mvsU distCompl
              in (mvsK U.++ succK2, mvsU U.++ succU2, a4)
            processUnknown (succU2, a2) posInt =
              let pos = PointArray.aunindex a2 posInt
                  fUnknown lU moveInt =
                    let pInt = posInt + moveInt
                        p = PointArray.aunindex a2 pInt
                        freshMv = a2 `PointArray.atInt` pInt == apartBfs
                        notBlocked = passUnknown pos p
                    in if freshMv && notBlocked
                       then U.cons pInt lU
                       else lU
                  mvsU = U.foldl' fUnknown U.empty movesV
                  a3 = PointArray.updateConst a2 mvsU distCompl
              in (mvsU U.++ succU2, a3)
            (succU5, !a5) = U.foldl' processUnknown (U.empty, a) predU
            (succK6, succU6, !a6) =
              U.foldl' processKnown (U.empty, succU5, a5) predK
        in if distance == predMaxKnownBfs  -- wasting one Known slot
           then a6  -- too far
           else if U.null succK6 && U.null succU6
                then a6  -- no more dungeon positions to check
                else bfs (succ distance) succK6 succU6 a6
  in PointArray.forceA  -- no more modifications of this array
     $ bfs (succ minKnownBfs)
           (U.singleton $ PointArray.aindex aInitial origin) U.empty
           (aInitial PointArray.// [(origin, minKnownBfs)])

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
{-# INLINE findPathBfs #-}
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

-- | Access a BFS array and interpret the looked up distance value.
accessBfs :: PointArray.Array BfsDistance -> Point -> Maybe Int
{-# INLINE accessBfs #-}
accessBfs bfs target =
  let dist = bfs PointArray.! target
  in if dist == apartBfs
     then Nothing
     else Just $ fromEnum $ dist .&. complement minKnownBfs
