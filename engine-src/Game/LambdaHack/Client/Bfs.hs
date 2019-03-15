{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, RankNTypes,
             TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -ddump-simpl -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -ddump-to-file #-}
-- | Breadth first search algorithm.
module Game.LambdaHack.Client.Bfs
  ( BfsDistance, MoveLegal(..)
  , subtractBfsDistance, minKnownBfs, apartBfs, maxBfsDistance, fillBfs
  , AndPath(..), actorsAvoidedDist, findPathBfs
  , accessBfs
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , succBfsDistance, predBfsDistance, abortedUnknownBfs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Monad.ST.Strict (ST, runST)
import           Data.Binary
import           Data.Bits (Bits, complement, (.&.), (.|.))
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified Data.Primitive.PrimArray as PA
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import           GHC.Exts (inline)
import           GHC.Generics (Generic)

import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Core.Point
import qualified Game.LambdaHack.Core.PointArray as PointArray

-- | Weighted distance between points along shortest paths.
newtype BfsDistance = BfsDistance {bfsDistance :: Word8}
  deriving (Show, Eq, Ord, Bits)

instance PointArray.UnboxRepClass BfsDistance where
  type UnboxRep BfsDistance = Word8
  toUnboxRepUnsafe = bfsDistance
  fromUnboxRep = BfsDistance

-- | State of legality of moves between adjacent points.
data MoveLegal = MoveBlocked | MoveToOpen | MoveToClosed | MoveToUnknown
  deriving Eq

succBfsDistance :: BfsDistance -> BfsDistance
succBfsDistance d = BfsDistance $ bfsDistance d + 1

predBfsDistance :: BfsDistance -> BfsDistance
predBfsDistance d = BfsDistance $ bfsDistance d - 1

subtractBfsDistance :: BfsDistance -> BfsDistance -> Int
subtractBfsDistance d1 d2 = fromEnum $ bfsDistance d1 - bfsDistance d2

-- | The minimal distance value assigned to paths that don't enter
-- any unknown tiles.
minKnownBfs :: BfsDistance
minKnownBfs = BfsDistance 128

-- | The distance value that denotes no legal path between points,
-- either due to blocked tiles or pathfinding aborted at earlier tiles,
-- e.g., due to unknown tiles.
apartBfs :: BfsDistance
apartBfs = predBfsDistance minKnownBfs

-- | Maximum value of the type.
maxBfsDistance :: BfsDistance
maxBfsDistance = BfsDistance (maxBound :: Word8)

-- | The distance value that denotes that path search was aborted
-- at this tile due to too large actual distance
-- and that the tile was unknown.
-- It is also a true distance value for this tile.
abortedUnknownBfs :: BfsDistance
abortedUnknownBfs = predBfsDistance apartBfs

-- | Create and fill a BFS array for the given level.
-- Unsafe array operations are OK here, because the intermediate
-- values of the vector don't leak anywhere outside nor are kept unevaluated
-- and so they can't be overwritten by the unsafe side-effect.
--
-- When computing move cost, we assume doors openable at no cost,
-- because other actors use them, too, so the cost is shared and the extra
-- visiblity is valuable, too. We treat unknown tiles specially.
-- Whether suspect tiles are considered openable depends on @smarkSuspect@.
--
-- Instead of a BFS queue (list) we use these two arrays, for (JS) speed.
fillBfs :: PointArray.Array Word8
        -> Word8
        -> Point
        -> (PA.PrimArray Int, PA.PrimArray Int)
        -> PointArray.Array BfsDistance
{-# INLINE fillBfs #-}
fillBfs lalter alterSkill source (tabA, tabB) = runST $ do
  let arr = PointArray.replicateA
              (PointArray.axsize lalter) (PointArray.aysize lalter) apartBfs
  vThawed <- U.unsafeThaw $ PointArray.avector arr
  tabAThawed <- PA.unsafeThawPrimArray tabA
  tabBThawed <- PA.unsafeThawPrimArray tabB
  fillBfsThawed lalter alterSkill (fromEnum source)
                (tabAThawed, tabBThawed) vThawed
  void $ PA.unsafeFreezePrimArray tabAThawed
  void $ PA.unsafeFreezePrimArray tabBThawed
  void $ U.unsafeFreeze vThawed
  return arr

-- The bangs are here only to get a sensible debug output of core.
fillBfsThawed :: forall s.
                 PointArray.Array Word8
              -> Word8
              -> PointI
              -> (PA.MutablePrimArray s Int, PA.MutablePrimArray s Int)
              -> U.MVector s Word8
              -> ST s ()
fillBfsThawed !lalter !alterSkill !sourceI
              (!tabAThawed, !tabBThawed) !vThawed = do
  let unsafeReadI :: PointI -> ST s BfsDistance
      {-# INLINE unsafeReadI #-}
      unsafeReadI p = BfsDistance <$> VM.unsafeRead vThawed p
      unsafeWriteI :: PointI -> BfsDistance -> ST s ()
      {-# INLINE unsafeWriteI #-}
      unsafeWriteI p c = VM.unsafeWrite vThawed p (bfsDistance c)
      bfs :: PA.MutablePrimArray s Int
          -> PA.MutablePrimArray s Int
          -> BfsDistance
          -> Int
          -> ST s ()
      bfs !tabReadThawed !tabWriteThawed !distance !prevMaxPosIx = do
        let unsafeReadCurrent :: Int -> ST s PointI
            {-# INLINE unsafeReadCurrent #-}
            unsafeReadCurrent ix = PA.readPrimArray tabReadThawed ix
            unsafeWriteNext :: Int -> PointI -> ST s ()
            {-# INLINE unsafeWriteNext #-}
            unsafeWriteNext ix p = PA.writePrimArray tabWriteThawed ix p
            processKnown :: Int -> Int -> ST s Int
            processKnown !posIx !acc1 =
              if posIx == -1
              then return acc1  -- all queued positions inspected
              else do
                pos <- unsafeReadCurrent posIx
                let fKnown :: (X, Y) -> Int -> ST s Int
                    {-# INLINE fKnown #-}
                    fKnown move acc2 = do
                      let p = pos + inline fromEnum (uncurry Vector move)
                      pDist <- unsafeReadI p
                      if pDist /= apartBfs
                      then return acc2  -- the position visited already
                      else do
                        let alter :: Word8
                            !alter = lalter `PointArray.accessI` p
                        if | alterSkill < alter -> return acc2
                           | alter == 1 -> do
                             let distCompl = distance .&. complement minKnownBfs
                             unsafeWriteI p distCompl
                             return acc2
                           | otherwise -> do
                             unsafeWriteI p distance
                             unsafeWriteNext acc2 p
                             return $! acc2 + 1
                -- Innermost loop over @moves@ manually unrolled for (JS) speed:
                return acc1
                  >>= fKnown (-1, -1)
                  >>= fKnown (0, -1)
                  >>= fKnown (1, -1)
                  >>= fKnown (1, 0)
                  >>= fKnown (1, 1)
                  >>= fKnown (0, 1)
                  >>= fKnown (-1, 1)
                  >>= fKnown (-1, 0)
                  >>= processKnown (posIx - 1)
        acc3 <- processKnown (prevMaxPosIx - 1) 0
        let distanceNew = succBfsDistance distance
        if acc3 == 0 || distanceNew == maxBfsDistance
        then return () -- no more close enough dungeon positions
        else bfs tabWriteThawed tabReadThawed distanceNew acc3
  VM.unsafeWrite vThawed sourceI (bfsDistance minKnownBfs)
  PA.writePrimArray tabAThawed 0 sourceI
  bfs tabAThawed tabBThawed (succBfsDistance minKnownBfs) 1

data AndPath = AndPath
  { pathSource :: Point    -- never included in @pathList@
  , pathList   :: [Point]
  , pathGoal   :: Point    -- needn't be @last pathList@
  , pathLen    :: Int      -- needn't be @length pathList@
  }
  deriving (Show, Generic)

instance Binary AndPath

actorsAvoidedDist :: Int
actorsAvoidedDist = 5

-- | Find a path, without the source position, with the smallest length.
-- The @eps@ coefficient determines which direction (of the closest
-- directions available) that path should prefer, where 0 means north-west
-- and 1 means north. The path tries hard to avoid actors and tries to avoid
-- tiles that need altering and ambient light. Actors are avoided only close
-- to the start of the path, because elsewhere they are likely to move
-- before they are reached. Even projectiles are avoided,
-- which sometimes has the effect of choosing a safer route
-- (regardless if the projectiles are friendly fire or not).
--
-- An unwelcome side effect of avoiding actors is that friends will sometimes
-- avoid displacing and instead perform two separate moves, wasting 1 turn
-- in total. But in corridors they will still displace and elsewhere
-- this scenario was quite rare already.
findPathBfs :: BigActorMap -> PointArray.Array Word8 -> (PointI -> Bool)
            -> Point -> Point -> Int
            -> PointArray.Array BfsDistance
            -> Maybe AndPath
{-# INLINE findPathBfs #-}
findPathBfs lbig lalter fovLit pathSource pathGoal sepsRaw
            arr@PointArray.Array{..} =
  let !pathGoalI = fromEnum pathGoal
      !pathSourceI = fromEnum pathSource
      eps = sepsRaw `mod` 4
      (mc1, mc2) = splitAt eps movesCardinalI
      (md1, md2) = splitAt eps movesDiagonalI
      -- Prefer cardinal directions when closer to the target, so that
      -- the enemy can't easily disengage.
      prefMoves = mc2 ++ reverse mc1 ++ md2 ++ reverse md1  -- fuzz
      track :: PointI -> BfsDistance -> [Point] -> [Point]
      track !pos !oldDist !suffix | oldDist == minKnownBfs =
        assert (pos == pathSourceI) suffix
      track pos oldDist suffix | oldDist == succBfsDistance minKnownBfs =
        let !posP = toEnum pos
        in posP : suffix  -- avoid calculating minP and dist for the last call
      track pos oldDist suffix =
        let !dist = predBfsDistance oldDist
            minChild :: PointI -> Bool -> Word8 -> [VectorI] -> PointI
            minChild !minP _ _ [] = minP
            minChild minP maxDark minAlter (mv : mvs) =
              let !p = pos + mv
                  backtrackingMove =
                    BfsDistance (arr `PointArray.accessI` p) /= dist
              in if backtrackingMove
                 then minChild minP maxDark minAlter mvs
                 else let free = fromEnum (bfsDistance dist) < actorsAvoidedDist
                                 || p `IM.notMember` EM.enumMapToIntMap lbig
                          alter | free = lalter `PointArray.accessI` p
                                | otherwise = maxBound-1  -- occupied; disaster
                          dark = not $ fovLit p
                      -- Prefer paths without actors and through
                      -- more easily opened tiles and, secondly,
                      -- in the ambient dark (even if light carried,
                      -- because it can be taken off at any moment).
                      in if | alter == 0 && dark -> p  -- speedup
                            | alter < minAlter -> minChild p dark alter mvs
                            | dark > maxDark && alter == minAlter ->
                              minChild p dark alter mvs
                            | otherwise -> minChild minP maxDark minAlter mvs
            -- @maxBound@ means not alterable, so some child will be lower
            !newPos = minChild pos{-dummy-} False maxBound prefMoves
#ifdef WITH_EXPENSIVE_ASSERTIONS
            !_A = assert (newPos /= pos) ()
#endif
            !posP = toEnum pos
        in track newPos dist (posP : suffix)
      !goalDist = BfsDistance $ arr `PointArray.accessI` pathGoalI
      pathLen = fromEnum $ bfsDistance $ goalDist .&. complement minKnownBfs
      pathList = track pathGoalI (goalDist .|. minKnownBfs) []
      andPath = AndPath{..}
  in assert (BfsDistance (arr `PointArray.accessI` pathSourceI)
             == minKnownBfs) $
     if goalDist /= apartBfs && pathLen < 2 * chessDist pathSource pathGoal
     then Just andPath
     else let f :: (Point, Int, Int, Int) -> Point -> BfsDistance
                -> (Point, Int, Int, Int)
              f acc@(pAcc, dAcc, chessAcc, sumAcc) p d =
                if d <= abortedUnknownBfs  -- works in visible secrets mode only
                   || d /= apartBfs && adjacent p pathGoal  -- works for stairs
                then let dist = fromEnum $ bfsDistance
                                $ d .&. complement minKnownBfs
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
             then if goalDist /= apartBfs then Just andPath else Nothing
             else let pathList2 =
                        track (fromEnum pRes)
                              (BfsDistance (toEnum dRes) .|. minKnownBfs) []
                  in Just AndPath{pathList = pathList2, pathLen = sumRes, ..}

-- | Access a BFS array and interpret the looked up distance value.
accessBfs :: PointArray.Array BfsDistance -> Point -> Maybe Int
accessBfs bfs p =
  let dist = bfs PointArray.! p
  in if dist == apartBfs
     then Nothing
     else Just $ fromEnum $ bfsDistance $ dist .&. complement minKnownBfs
