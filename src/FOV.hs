module FOV where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Ratio
import Debug.Trace

import Geometry
import Level

type Interval = (Rational, Rational)
type Distance = Int
type Progress = Int

-- | Recursive Shadow Casting FOV with infinite visibility range.
-- It's not tuned enough for special cases, such as visibility through
-- a diagonal line of walls (this appears in LambdaHack only
-- when two corridors touch diagonally by accident).

-- | The current state of a scan is kept in a variable of Maybe Rational.
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
scan :: ((Distance,Progress) -> Loc) -> LMap -> Distance -> Interval -> Set Loc
scan tr l d (s,e) =
    let ps = downBias (s * fromIntegral d)   -- minimal progress to check
        pe = upBias (e * fromIntegral d)     -- maximal progress to check
        st = if open (l `at` tr (d,ps)) then Just s   -- start in light
                                        else Nothing  -- start in shadow
    in
        -- trace (show (d,s,e,ps,pe)) $
        S.union (S.fromList [tr (d,p) | p <- [ps..pe]]) (scan' st ps pe)
  where
    scan' :: Maybe Rational -> Progress -> Progress -> Set Loc
    -- scan' st ps pe
    --   | trace (show (st,ps,pe)) False = undefined
    scan' (Just s) ps pe
      | s  >= e  = S.empty               -- empty interval
      | ps > pe  = scan tr l (d+1) (s,e) -- reached end, scan next
      | closed (l `at` tr (d,ps)) =
                   let ne = (fromIntegral ps - (1%2)) / (fromIntegral d + (1%2))
                   in  scan tr l (d+1) (s,ne) `S.union` scan' Nothing (ps+1) pe
                                      -- entering shadow
      | otherwise = scan' (Just s) (ps+1) pe
                                      -- continue in light
    scan' Nothing ps pe
      | ps > pe  = S.empty            -- reached end while in shadow
      | open (l `at` tr (d,ps)) =
                   let ns = (fromIntegral ps - (1%2)) / (fromIntegral d - (1%2))
                   in  scan' (Just ns) (ps+1) pe
                                      -- moving out of shadow
      | otherwise = scan' Nothing (ps+1) pe
                                      -- continue in shadow

tr0 (oy,ox) (d,p) = (oy + d,ox + p)
tr1 (oy,ox) (d,p) = (oy + d,ox - p)
tr2 (oy,ox) (d,p) = (oy - d,ox + p)
tr3 (oy,ox) (d,p) = (oy - d,ox - p)
tr4 (oy,ox) (d,p) = (oy + p,ox + d)
tr5 (oy,ox) (d,p) = (oy + p,ox - d)
tr6 (oy,ox) (d,p) = (oy - p,ox + d)
tr7 (oy,ox) (d,p) = (oy - p,ox - d)

downBias, upBias :: (Integral a, Integral b) => Ratio a -> b
downBias x = round (x - 1 % (denominator x * 3))
upBias   x = round (x + 1 % (denominator x * 3))


-- | Perform a full scan for a given location. Returns the locations
-- that are currently visible.
fullscan :: Maybe Int -> Loc -> LMap -> Set Loc
fullscan range loc lmap =
  case range of
    Nothing ->  -- shadow casting with infinite range
      S.unions $
      L.map (\ tr ->
              scan (tr loc) lmap 0 (0,1)) [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Just n  ->  -- precise permissive with range n
      S.unions $
      L.map (\ tr ->
              pscan n (tr loc) lmap 0 (0,1)) [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]


-- Precise Permissive FOV with a given range (TODO)

-- | The current state of a scan is kept in a variable of Maybe Rational.
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
pscan :: Distance -> ((Distance, Progress) -> Loc) -> LMap ->
         Distance -> Interval -> Set Loc
pscan n tr l d (s, e) =
  let ps = downBias (s * fromIntegral d)  -- minimal progress to check
      pe = upBias (e * fromIntegral d)    -- maximal progress to check
      start = if open (l `at` tr (d, ps))
              then Just s   -- start in light
              else Nothing  -- start in shadow
  in
   -- trace (show (d,s,e,ps,pe)) $
   S.union
     (S.fromList [tr (d, p) | p <- [ps..pe]])
     (pscan' start ps pe)
     where
       pscan' :: Maybe Rational -> Progress -> Progress -> Set Loc
       -- pscan' start ps pe
       --   | trace (show (start,ps,pe)) False = undefined
       pscan' (Just s) ps pe
         | s >= e    = S.empty                    -- empty interval
         | ps > pe   = pscan n tr l (d+1) (s, e)  -- reached end, scan next
         | closed (l `at` tr (d, ps)) =           -- entering shadow
             let ne = (fromIntegral ps - (1%2)) / (fromIntegral d + (1%2))
             in  S.union
                   (pscan n tr l (d+1) (s, ne))
                   (pscan' Nothing (ps+1) pe)
         | otherwise =                            -- continue in light
             pscan' (Just s) (ps+1) pe

       pscan' Nothing ps pe
         | ps > pe   = S.empty                    -- reached end while in shadow
         | open (l `at` tr (d, ps)) =             -- moving out of shadow
             let ns = (fromIntegral ps - (1%2)) / (fromIntegral d - (1%2))
             in  pscan' (Just ns) (ps+1) pe
         | otherwise =                            -- continue in shadow
             pscan' Nothing (ps+1) pe
