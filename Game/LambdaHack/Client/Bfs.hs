{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Breadth first search algorithms.
module Game.LambdaHack.Client.Bfs
  ( BfsDistance, MoveLegal(..), minKnownBfs, apartBfs, fillBfs
  , AndPath(..), findPathBfs
  , accessBfs
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Monad.ST.Strict
import Data.Binary
import Data.Bits (Bits, complement, (.&.), (.|.))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Weighted distance between points along shortest paths.
newtype BfsDistance = BfsDistance {bfsDistance :: Word8}
  deriving (Show, Eq, Ord, Enum, Bounded, Bits)

-- | State of legality of moves between adjacent points.
data MoveLegal = MoveBlocked | MoveToOpen | MoveToClosed | MoveToUnknown
  deriving Eq

-- | The minimal distance value assigned to paths that don't enter
-- any unknown tiles.
minKnownBfs :: BfsDistance
minKnownBfs = toEnum $ (1 + fromEnum (maxBound :: BfsDistance)) `div` 2

-- | The distance value that denotes no legal path between points,
-- either due to blocked tiles or pathfinding aborted at earlier tiles,
-- e.g., due to unknown tiles.
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

type PointI = Int

type VectorI = Int

-- | Fill out the given BFS array.
-- Unsafe @PointArray@ operations are OK here, because the intermediate
-- values of the vector don't leak anywhere outside nor are kept unevaluated
-- and so they can't be overwritten by the unsafe side-effect.
--
-- When computing move cost, we assume doors openable at no cost,
-- because other actors use them, too, so the cost is shared and the extra
-- visiblity is valuable, too. We treat unknown tiles specially.
-- Whether suspect tiles are considered openable depends on @smarkSuspect@.
fillBfs :: PointArray.Array Word8
        -> Word8
        -> Point                          -- ^ starting position
        -> PointArray.Array BfsDistance   -- ^ initial array, with @apartBfs@
        -> ()
{-# INLINE fillBfs #-}
fillBfs lalter alterSkill source arr@PointArray.Array{..} =
  let vToI (x, y) = PointArray.pindex axsize (Point x y)
      movesI :: [VectorI]
      movesI = map vToI
        [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]
      unsafeWriteI :: Int -> BfsDistance -> ()
      {-# INLINE unsafeWriteI #-}
      unsafeWriteI p c = runST $ do
        vThawed <- U.unsafeThaw avector
        VM.unsafeWrite vThawed p (bfsDistance c)
        void $ U.unsafeFreeze vThawed
      bfs :: BfsDistance -> [PointI] -> ()  -- modifies the vector
      bfs !distance !predK =
        let processKnown :: PointI -> [PointI] -> [PointI]
            processKnown !pos !succK2 =
              -- Terrible hack trigger warning!
              -- Unsafe ops inside @fKnown@ seem to be OK, for no particularly
              -- clear reason. The array value given to each p depends on
              -- array value only at p (it's not overwritten if already there).
              -- So the only problem with the unsafe ops writing at p is
              -- if one with higher depth (dist) is evaluated earlier
              -- than another with lower depth. The particular pattern of
              -- laziness and order of list elements below somehow
              -- esures the lowest possible depth is always written first.
              -- The code also doesn't keep a wholly evaluated list of all p
              -- at a given depth, but generates them on demand, unlike a fully
              -- strict version inside the ST monad. So it uses little memory
              -- and is fast.
              let fKnown :: [PointI] -> VectorI -> [PointI]
                  fKnown !l !move =
                    let !p = pos + move
                        visitedMove =
                          BfsDistance (arr `PointArray.accessI` p) /= apartBfs
                    in if visitedMove
                       then l
                       else let alter :: Word8
                                !alter = lalter `PointArray.accessI` p
                            in if | alterSkill < alter -> l
                                  | alter == 1 ->
                                      let distCompl =
                                            distance .&. complement minKnownBfs
                                      in unsafeWriteI p distCompl
                                         `seq` l
                                  | otherwise -> unsafeWriteI p distance
                                                 `seq` p : l
              in foldl' fKnown succK2 movesI
            succK4 = foldr processKnown [] predK
        in if null succK4 || distance == abortedKnownBfs
           then () -- no more dungeon positions to check, or we delved too deep
           else bfs (succ distance) succK4
  in bfs (succ minKnownBfs) [PointArray.pindex axsize source]

data AndPath =
    AndPath { pathList :: [Point]
            , pathGoal :: Point    -- needn't be @last pathList@
            , pathLen  :: Int      -- needn't be @length pathList@
            }
  | NoPath
  deriving (Show, Generic)

instance Binary AndPath

-- | Find a path, without the source position, with the smallest length.
-- The @eps@ coefficient determines which direction (of the closest
-- directions available) that path should prefer, where 0 means north-west
-- and 1 means north.
findPathBfs :: PointArray.Array Word8 -> (Point -> Bool)
            -> Point -> Point -> Int
            -> PointArray.Array BfsDistance
            -> AndPath
{-# INLINE findPathBfs #-}
findPathBfs lalter fovLit pathSource pathGoal sepsRaw
            arr@PointArray.Array{..} =
  let !pathGoalI = PointArray.pindex axsize pathGoal
      !pathSourceI = PointArray.pindex axsize pathSource
      eps = sepsRaw `mod` 4
      (mc1, mc2) = splitAt eps [(0, -1), (1, 0), (0, 1), (-1, 0)]
      (md1, md2) = splitAt eps [(-1, -1), (1, -1), (1, 1), (-1, 1)]
      -- Prefer cardinal directions when closer to the target, so that
      -- the enemy can't easily disengage (open/unknown below overrides that).
      prefMoves = mc1 ++ reverse mc2 ++ md2 ++ reverse md1  -- fuzz
      vToI (x, y) = PointArray.pindex axsize (Point x y)
      movesI :: [VectorI]
      movesI = map vToI prefMoves
      track :: PointI -> BfsDistance -> [Point] -> [Point]
      track !pos !oldDist !suffix | oldDist == minKnownBfs =
        assert (pos == pathSourceI) suffix
      track pos oldDist suffix | oldDist == succ minKnownBfs =
        let !posP = PointArray.punindex axsize pos
        in posP : suffix  -- avoid calculating minP and dist for the last call
      track pos oldDist suffix =
        let !dist = pred oldDist
            minChild !minP _ _ [] = minP
            minChild minP maxDark minAlter (mv : mvs) =
              let !p = pos + mv
                  backtrackingMove =
                    BfsDistance (arr `PointArray.accessI` p) /= dist
              in if backtrackingMove
                 then minChild minP maxDark minAlter mvs
                 else let alter = lalter `PointArray.accessI` p
                          dark = not $ fovLit $ PointArray.punindex axsize p
                      -- Prefer paths through more easily opened tiles
                      -- and, secondly, in the ambient dark (even if light
                      -- carried, because it can be taken off at any moment).
                      in if | alter == 0 && dark -> p  -- speedup
                            | alter < minAlter -> minChild p dark alter mvs
                            | dark > maxDark && alter == minAlter ->
                              minChild p dark alter mvs
                            | otherwise -> minChild minP maxDark minAlter mvs
            -- @maxBound@ means not alterable, so some child will be lower
            !newPos = minChild pos{-dummy-} False maxBound movesI
#ifdef WITH_EXPENSIVE_ASSERTIONS
            !_A = assert (newPos /= pos) ()
#endif
            !posP = PointArray.punindex axsize pos
        in track newPos dist (posP : suffix)
      !goalDist = BfsDistance $ arr `PointArray.accessI` pathGoalI
      pathLen = fromEnum $ goalDist .&. complement minKnownBfs
      pathList = track pathGoalI (goalDist .|. minKnownBfs) []
      andPath = AndPath{..}
  in assert (BfsDistance (arr `PointArray.accessI` pathSourceI)
             == minKnownBfs) $
     if goalDist /= apartBfs && pathLen < 2 * chessDist pathSource pathGoal
     then andPath
     else let f :: (Point, Int, Int, Int) -> Point -> BfsDistance
                -> (Point, Int, Int, Int)
              f acc@(pAcc, dAcc, chessAcc, sumAcc) p d =
                if d <= abortedUnknownBfs  -- works in visible secrets mode only
                   || d /= apartBfs && adjacent p pathGoal  -- works for stairs
                then let dist = fromEnum $ d .&. complement minKnownBfs
                         chessNew = chessDist p pathGoal
                         sumNew = dist + 2 * chessNew
                         resNew = (p, dist, chessNew, sumNew)
                     in case compare sumNew sumAcc of
                       LT -> resNew
                       EQ -> case compare chessNew chessAcc of
                         LT -> resNew
                         EQ -> case compare dist dAcc of
                           LT -> resNew
                           EQ | euclidDistSq p pathGoal
                                < euclidDistSq pAcc pathGoal -> resNew
                           _ -> acc
                         _ -> acc
                       _ -> acc
                else acc
              initAcc = (originPoint, maxBound, maxBound, maxBound)
              (pRes, dRes, _, sumRes) = PointArray.ifoldlA' f initAcc arr
          in if sumRes == maxBound
                || goalDist /= apartBfs && pathLen < sumRes
             then if goalDist /= apartBfs then andPath else NoPath
             else let pathList2 = track (PointArray.pindex axsize pRes)
                                        (toEnum dRes .|. minKnownBfs) []
                  in AndPath{pathList = pathList2, pathLen = sumRes, ..}

-- | Access a BFS array and interpret the looked up distance value.
accessBfs :: PointArray.Array BfsDistance -> Point -> Maybe Int
accessBfs bfs p =
  let dist = bfs PointArray.! p
  in if dist == apartBfs
     then Nothing
     else Just $ fromEnum $ dist .&. complement minKnownBfs
