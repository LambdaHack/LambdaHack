module FOV.Permissive where

import Data.Set as S

import FOV.Common
import Geometry
import Level
import qualified Tile

-- Permissive FOV with a given range.

-- | PFOV, clean-room reimplemented based on the algorithm described in http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View,
-- though the general structure is more influenced by recursive shadow casting,
-- as implemented in Shadow.hs. In the result, this algorithm is much faster
-- than the original algorithm on dense maps, since it does not scan
-- areas blocked by shadows.
-- See https://github.com/Mikolaj/LambdaHack/wiki/Fov-and-los
-- for some more context.

-- TODO: Scanning squares on horizontal lines in octants, not squares
-- on diagonals in quadrants, may be much faster and a bit simpler.
-- Right now we build new view on each end of each visible wall tile
-- and this is necessary only for straight, thin, diagonal walls.

-- | The current state of a scan is kept in Maybe (Line, ConvexHull).
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
pscan :: Distance -> (Bump -> Loc) -> LMap -> Distance -> EdgeInterval ->
         Set Loc
pscan r ptr l d (s@(sl{-shallow line-}, sBumps), e@(el{-steep line-}, eBumps)) =
  -- trace (show (d,s,e,ps,pe)) $
  if illegal
  then S.empty
  else S.union outside (S.fromList [tr (d, p) | p <- [ps..pe]])
    -- the area is diagonal, which is incorrect, but looks good enough
    where
      (ns, ks) = pintersect sl d
      (ne, ke) = pintersect el d
      -- Corners are translucent, so they are invisible, so if intersection
      -- is at a corner, choose pe that creates the smaller view.
      (ps, pe) = (ns `div` ks, ne `divUp` ke - 1)  -- progress interval to check
      -- Single ray from an extremity, produces non-permissive digital lines.
      illegal  = let (n, k) = pintersect sl 0
                 in  ns*ke == ne*ks && (n == 0 || n == k)
      outside
        | d >= r = S.empty
        | Tile.isClear (l `at` tr (d, ps)) =         -- start in light
            pscan' (Just s) ps
        | ps == ns `divUp` ks = pscan' (Just s) ps   -- start in a corner
        | otherwise = pscan' Nothing (ps+1)          -- start in mid-wall

      dp2bump     (d, p) = B(p, d - p)
      bottomRight (d, p) = B(p, d - p + 1)
      tr = ptr . dp2bump

      pscan' :: Maybe Edge -> Progress -> Set Loc
      pscan' (Just s@(_, sBumps)) ps
        | ps > pe =                                  -- reached end, scan next
            pscan r ptr l (d+1) (s, e)
        | not $ Tile.isClear (l `at` tr (d, ps)) =   -- enter shadow, steep bump
            let steepBump = bottomRight (d, ps)
                gte = flip $ psteeper steepBump
                -- sBumps may contain steepBump, but maximal will ignore it
                nep = maximal gte sBumps
                neBumps = addHull gte steepBump eBumps
            in  S.union
                  (pscan r ptr l (d+1) (s, (pline nep steepBump, neBumps)))
                  (pscan' Nothing (ps+1))
        | otherwise = pscan' (Just s) (ps+1)   -- continue in light

      pscan' Nothing ps
        | ps > ne `div` ke = S.empty           -- reached absolute end
        | otherwise =                          -- out of shadow, shallow bump
            -- the light can be just through a corner of diagonal walls
            -- and the recursive call verifies that at the same ps coordinate
            let shallowBump = bottomRight (d, ps)
                gte = psteeper shallowBump
                nsp = maximal gte eBumps
                nsBumps = addHull gte shallowBump sBumps
            in  pscan' (Just (pline nsp shallowBump, nsBumps)) ps

      pline p1 p2 =
        pdebugLine $  -- TODO: disable when it becomes a bottleneck
        (p1, p2)

      psteeper f p1 p2 =
        pdebugSteeper f p1 p2 $  -- TODO: disable when it becomes a bottleneck
        steeper f p1 p2

-- | The y coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of squares at distance d from (0, 0).
pintersect :: Line -> Distance -> (Int, Int)
pintersect (B(y, x), B(yf, xf)) d =
  ((1 + d)*(yf - y) + y*xf - x*yf, (xf - x) + (yf - y))
{-
Derivation of the formula:
The intersection point (yt, xt) satisfies the following equalities:
xt = 1 + d - yt
(yt - y) (xf - x) = (xt - x) (yf - y)
hence
(yt - y) (xf - x) = (xt - x) (yf - y)
yt (xf - x) - y xf = xt (yf - y) - x yf
yt (xf - x) - y xf = (1 + d) (yf - y) - yt (yf - y) - x yf
yt (xf - x) + yt (yf - y) = (1 + d) (yf - y) - x yf + y xf
yt = ((1 + d) (yf - y) + y xf - x yf) / (xf - x + yf - y)

General remarks:
A square is denoted by its bottom-left corner. Hero at (0,0).
Order of processing in the first quadrant is
9
58
247
@136
so the first processed square is at (0, 1). The order is reversed
wrt the shadow casting algorithm above. The line in the curent state
of scan' is not the steep line, but the shallow line,
and we start scanning from the bottom right.

The Loc coordinates are cartesian, the Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and rotated so that he always
looks at the first quadrant, the (Distance, Progress) cordinates
are mangled and not used for geometry.
-}

-- | Debug functions for PFOV:

-- | Debug: calculate steeper for PFOV in another way and compare results.
pdebugSteeper :: Bump ->  Bump -> Bump -> Bool -> Bool
pdebugSteeper f p1 p2 x =
  let (n1, k1) = pintersect (p1, f) 0
      (n2, k2) = pintersect (p2, f) 0
  in  if x == (n1 * k2 <= k1 * n2)
      then x
      else error $ "psteeper: " ++ show (f, p1, p2, x)

-- | Debug: checks postconditions of borderLine.
pdebugLine :: Line -> Line
pdebugLine line@(B(y1, x1), B(y2, x2))
  | y1 == y2 && x1 == x2 =
      error $ "pdebugLine: wrongly defined line " ++ show line
  | x2 - x1 == - (y2 - y1) =
      error $ "pdebugLine: diagonal line " ++ show line
  | crossL0 =
      error $ "pdebugLine: crosses diagonal below 0 " ++ show line
  | crossG1 =
      error $ "pdebugLine: crosses diagonal above 1 " ++ show line
  | otherwise = line
    where
      (n, k)  = pintersect line 0
      (q, r)  = if k == 0 then (0, 0) else n `divMod` k
      crossL0 = q < 0  -- q truncated toward negative infinity
      crossG1 = q >= 1 && (q > 1 || r /= 0)
