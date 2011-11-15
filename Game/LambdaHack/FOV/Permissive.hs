module Game.LambdaHack.FOV.Permissive (scan) where

import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.FOV.Common
import Game.LambdaHack.Loc
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Content.TileKind

-- Permissive FOV with a given range.

-- | PFOV, clean-room reimplemented based on the algorithm described in http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View,
-- though the general structure is more influenced by recursive shadow casting,
-- as implemented in Shadow.hs. In the result, this algorithm is much faster
-- than the original algorithm on dense maps, since it does not scan
-- areas blocked by shadows.
-- See https://github.com/Mikolaj/Allure/wiki/Fov-and-los
-- for some more context.

-- TODO: Scanning squares on horizontal lines in octants, not squares
-- on diagonals in quadrants, may be much faster and a bit simpler.
-- Right now we build new view on each end of each visible wall tile
-- and this is necessary only for straight, thin, diagonal walls.

-- | The current state of a scan is kept in Maybe (Line, ConvexHull).
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
scan :: Distance -> (Bump -> Loc) -> Kind.Ops TileKind -> Level -> S.Set Loc
scan r tr cops l =
  -- the area is diagonal, which is incorrect, but looks good enough
  dscan 1 (((B(0, 1), B(r+1, 0)), [B(1, 0)]), ((B(1, 0), B(0, r+1)), [B(0, 1)]))
 where
  dscan :: Distance -> EdgeInterval -> S.Set Loc
  dscan d (s0@(sl{-shallow line-}, sBumps0), e@(el{-steep line-}, eBumps)) =
    assert (r >= d && d >= 0 && pe + 1 >= ps0 && ps0 >= 0
            `blame` (r,d,s0,e,ps0,pe)) $
    if illegal then S.empty else S.union inside outside
   where
    (ns, ks) = intersect sl d
    (ne, ke) = intersect el d
    -- Corners are translucent, so they are invisible, so if intersection
    -- is at a corner, choose pe that creates the smaller view.
    (ps0, pe) = (ns `div` ks, ne `divUp` ke - 1)  -- progress interval to check
    -- Single ray from an extremity, produces non-permissive digital lines.
    illegal  = let (n, k) = intersect sl 0
               in ns*ke == ne*ks && (n `elem` [0, k])
    pd2bump     (p, di) = B(di - p    , p)
    bottomRight (p, di) = B(di - p + 1, p)
    isClear :: Bump -> Bool
    isClear = Tile.isClear cops . (l `at`) . tr

    inside = S.fromList [tr (pd2bump (p, d)) | p <- [ps0..pe]]
    outside
      | d >= r = S.empty
      | isClear (pd2bump (ps0, d)) = mscan (Just s0) ps0  -- start in light
      | ps0 == ns `divUp` ks = mscan (Just s0) ps0          -- start in a corner
      | otherwise = mscan Nothing (ps0+1)                   -- start in mid-wall

    mscan :: Maybe Edge -> Progress -> S.Set Loc
    mscan (Just s@(_, sBumps)) ps
      | ps > pe = dscan (d+1) (s, e)            -- reached end, scan next
      | not $ isClear (pd2bump (ps, d)) =  -- enter shadow, steep bump
          let steepBump = bottomRight (ps, d)
              gte = flip $ dsteeper steepBump
              -- sBumps may contain steepBump, but maximal will ignore it
              nep = maximal gte sBumps
              neBumps = addHull gte steepBump eBumps
          in S.union (dscan (d+1) (s, (dline nep steepBump, neBumps)))
                     (mscan Nothing (ps+1))
      | otherwise = mscan (Just s) (ps+1)       -- continue in light

    mscan Nothing ps
      | ps > ne `div` ke = S.empty              -- reached absolute end
      | otherwise =                             -- out of shadow, shallow bump
          -- the light can be just through a corner of diagonal walls
          -- and the recursive call verifies that at the same ps coordinate
          let shallowBump = bottomRight (ps, d)
              gte = dsteeper shallowBump
              nsp = maximal gte eBumps
              nsBumps = addHull gte shallowBump sBumps0
          in mscan (Just (dline nsp shallowBump, nsBumps)) ps

-- | Create a line from two points. Debug: check if well-defined.
dline :: Bump -> Bump -> Line
dline p1 p2 =
  assert (uncurry blame $ debugLine (p1, p2)) $
  (p1, p2)

-- | Compare steepness of (p1, f) and (p2, f).
-- Debug: Verify that the results of 2 independent checks are equal.
dsteeper :: Bump ->  Bump -> Bump -> Bool
dsteeper f p1 p2 =
  assert (res == debugSteeper f p1 p2) $
  res
   where res = steeper f p1 p2

-- | The y coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of squares at distance d from (0, 0).
intersect :: Line -> Distance -> (Int, Int)
intersect (B(x, y), B(xf, yf)) d =
  assert (allB (>= 0) [x, y, xf, yf]) $
  ((1 + d)*(yf - y) + y*xf - x*yf, (xf - x) + (yf - y))
{-
Derivation of the formula:
The intersection point (xt, yt) satisfies the following equalities:
xt = 1 + d - yt
(yt - y) (xf - x) = (xt - x) (yf - y)
hence
(yt - y) (xf - x) = (xt - x) (yf - y)
yt (xf - x) - y xf = xt (yf - y) - x yf
yt (xf - x) - y xf = (1 + d) (yf - y) - yt (yf - y) - x yf
yt (xf - x) + yt (yf - y) = (1 + d) (yf - y) - x yf + y xf
yt = ((1 + d) (yf - y) + y xf - x yf) / (xf - x + yf - y)

General remarks:
A square is denoted by its bottom-left corner. Hero at (0, 0).
Order of processing in the first quadrant is
9
58
247
@136
so the first processed square is at (0, 1). The order is reversed
wrt the restrictive shadow casting algorithm. The line in the curent state
of mscan is not the steep line, but the shallow line,
and we start scanning from the bottom right.

The Loc coordinates are cartesian. The Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and rotated so that he always
looks at the first quadrant. The (Progress, Distance) cordinates
are mangled and not used for geometry.
-}

-- | Debug functions for PFOV:

-- | Debug: calculate steeper for PFOV in another way and compare results.
debugSteeper :: Bump -> Bump -> Bump -> Bool
debugSteeper f@(B(xf, yf)) p1@(B(x1, y1)) p2@(B(x2, y2)) =
  assert (allB (>= 0) [xf, yf, x1, y1, x2, y2]) $
  let (n1, k1) = intersect (p1, f) 0
      (n2, k2) = intersect (p2, f) 0
  in n1 * k2 <= k1 * n2

-- | Debug: checks postconditions of borderLine.
debugLine :: Line -> (Bool, String)
debugLine line@(B(x1, y1), B(x2, y2))
  | not (allB (>= 0) [x1, y1, x2, y2]) =
      (False, "negative coordinates: " ++ show line)
  | y1 == y2 && x1 == x2 =
      (False, "ill-defined line: " ++ show line)
  | x2 - x1 == - (y2 - y1) =
      (False, "diagonal line: " ++ show line)
  | crossL0 =
      (False, "crosses diagonal below 0: " ++ show line)
  | crossG1 =
      (False, "crosses diagonal above 1: " ++ show line)
  | otherwise = (True, "")
 where
  (n, k)  = intersect line 0
  (q, r)  = if k == 0 then (0, 0) else n `divMod` k
  crossL0 = q < 0  -- q truncated toward negative infinity
  crossG1 = q >= 1 && (q > 1 || r /= 0)
