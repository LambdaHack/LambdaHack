module FOV where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Ratio
import Debug.Trace

import Level

type Interval = (Rational, Rational)
type Distance = Int
type Progress = Int

-- The current state of a scan is kept in a variable of Maybe Rational.
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
scan :: ((Distance,Progress) -> Loc) -> LMap -> Distance -> Interval -> Set Loc
scan tr l d (s,e) = 
    let ps = downBias (s * fromIntegral d)   -- minimal progress to check
        pe = upBias (e * fromIntegral d)     -- maximal progress to check
        st = if open (l `at` tr (d,ps)) then (Just s) -- start in light
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

tr0 (oy,ox) (d,p) = (oy+d,ox+p)
tr1 (oy,ox) (d,p) = (oy+d,ox-p)
tr2 (oy,ox) (d,p) = (oy-d,ox+p)
tr3 (oy,ox) (d,p) = (oy-d,ox-p)
tr4 (oy,ox) (d,p) = (oy+p,ox+d)
tr5 (oy,ox) (d,p) = (oy+p,ox-d)
tr6 (oy,ox) (d,p) = (oy-p,ox+d)
tr7 (oy,ox) (d,p) = (oy-p,ox-d)

fullscan loc lvl = 
  S.unions $
  L.map (\ tr -> scan (tr loc) lvl 0 (0,1)) [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]


downBias, upBias :: (Integral a, Integral b) => Ratio a -> b
downBias x = round (x - 1 % (denominator x * 3))
upBias   x = round (x + 1 % (denominator x * 3))

test :: LMap
test = M.insert (3,3) (Rock,Unknown) $ 
       M.insert (3,1) (Rock,Unknown) $ 
       M.insert (6,7) (Rock,Unknown) $ 
       M.insert (7,8) (Rock,Unknown) $ 
       M.fromList [((x,y), (Floor,Unknown)) | x <- [0..10], y <- [0..10]]
