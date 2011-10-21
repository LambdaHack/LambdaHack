module FOV.Shadow where

import Data.Ratio
import qualified Data.Set as S

import Assert
import FOV.Common
import Geometry
import Level
import qualified Tile

-- Recursive Shadow Casting.

-- | A restrictive variant of Recursive Shadow Casting FOV with infinite range.
-- It's not designed for dungeons with diagonal walls, so they block visibility,
-- though they don't block movement. Such cases appear in the game only
-- when two corridors touch diagonally by accident and on the random pillars
-- levels.

{-
Field Of View
-------------

The algorithm used is a variant of Shadow Casting. We first compute
fields that are reachable (have unobstructed line of sight) from the hero's
position. Later, in Perception.hs,  from this information we compute
the fields that are visible (not hidden in darkness, etc.).

As input to the algorithm, we require information about fields that
block light. As output, we get information on the reachability of all fields.
We assume that the hero is located at position (0, 0)
and we only consider fields (line, row) where line >= 0 and 0 <= row <= line.
This is just about one eighth of the whole hero's surroundings,
but the other parts can be computed in the same fashion by mirroring
or rotating the given algorithm accordingly.

      fov (blocks, maxline) =
         shadow := \empty_set
         reachable (0, 0) := True
         for l \in [ 1 .. maxline ] do
            for r \in [ 0 .. l ] do
              reachable (l, r) := ( \exists a. a \in interval (l, r) \and
                                    a \not_in shadow)
              if blocks (l, r) then
                 shadow := shadow \union interval (l, r)
              end if
            end for
         end for
         return reachable

      interval (l, r) = return [ angle (l + 0.5, r - 0.5),
                                 angle (l - 0.5, r + 0.5) ]
      angle (l, r) = return atan (r / l)

The algorithm traverses the fields line by line, row by row.
At every moment, we keep in shadow the intervals which are in shadow,
measured by their angle. A square is reachable when any point
in it is not in shadow --- the algorithm is permissive in this respect.
We could also require that a certain fraction of the field is reachable,
or a specific point. Our choice has certain consequences. For instance,
a single blocking field throws a shadow, but the fields immediately behind
the blocking field are still visible.

We can compute the interval of angles corresponding to one square field
by computing the angle of the line passing the upper left corner
and the angle of the line passing the lower right corner.
This is what interval and angle do. If a field is blocking, the interval
for the square is added to the shadow set.
-}

-- | The current state of a scan is kept in a variable of Maybe Rational.
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
scan :: ((Distance, Progress) -> Loc) -> LMap -> Distance -> Interval
        -> S.Set Loc
scan tr l d (s0,e) =
    let ps = downBias (s0 * fromIntegral d)   -- minimal progress to check
        pe = upBias (e * fromIntegral d)     -- maximal progress to check
        st = if Tile.isClear (l `at` tr (d,ps))
             then Just s0   -- start in light
             else Nothing  -- start in shadow
    in
        -- traceShow (d,s,e,ps,pe) $
        assert (pe >= ps && ps >= 0 && d >= 0 && e >= 0 && s0 >= 0
                `blame` (d,s0,e,ps,pe)) $
        S.union (S.fromList [tr (d,p) | p <- [ps..pe]]) (scan' st ps pe)
  where
    scan' :: Maybe Rational -> Progress -> Progress -> S.Set Loc
    -- scan' st ps pe
    --   | traceShow (st,ps,pe) False = undefined
    scan' (Just s) ps pe
      | s  >= e  = S.empty               -- empty interval
      | ps > pe  = scan tr l (d+1) (s,e) -- reached end, scan next
      | not $ Tile.isClear (l `at` tr (d,ps)) =
                   let ne = (fromIntegral ps - (1%2)) / (fromIntegral d + (1%2))
                   in  scan tr l (d+1) (s,ne) `S.union` scan' Nothing (ps+1) pe
                                      -- entering shadow
      | otherwise = scan' (Just s) (ps+1) pe
                                      -- continue in light
    scan' Nothing ps pe
      | ps > pe  = S.empty            -- reached end while in shadow
      | Tile.isClear (l `at` tr (d,ps)) =
                   let ns = (fromIntegral ps - (1%2)) / (fromIntegral d - (1%2))
                   in  scan' (Just ns) (ps+1) pe
                                      -- moving out of shadow
      | otherwise = scan' Nothing (ps+1) pe
                                      -- continue in shadow

downBias, upBias :: (Integral a, Integral b) => Ratio a -> b
downBias x = round (x - 1 % (denominator x * 3))
upBias   x = round (x + 1 % (denominator x * 3))
