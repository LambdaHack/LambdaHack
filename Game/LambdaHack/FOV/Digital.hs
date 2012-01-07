-- | DFOV (Digital Field of View) implemented according to specification at <http://roguebasin.roguelikedevelopment.org/index.php?title=Digital_field_of_view_implementation>.
-- This fast version of the algorithm, based on "PFOV", have, AFAIK,
-- never been described nor implemented before.
-- See <https://github.com/Mikolaj/Allure/wiki/Fov-and-los>
-- for some more context and references.
module Game.LambdaHack.FOV.Digital (scan) where

import Game.LambdaHack.Geometry
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.FOV.Common

-- | Calculates the list of tiles visible from (0, 0).
scan :: Distance        -- ^ visiblity radius
     -> (Bump -> Bool)  -- ^ clear tile predicate
     -> [Bump]
scan r isClear =
  -- the scanned area is a square, which is a sphere in this metric; good
  dscan 1 (((B(1, 0), B(-r, r)), [B(0, 0)]), ((B(0, 0), B(r+1, r)), [B(1, 0)]))
 where
  dscan :: Distance -> EdgeInterval -> [Bump]
  dscan d (s0@(sl{-shallow line-}, sBumps0), e@(el{-steep line-}, eBumps)) =
    let ps0 = let (n, k) = intersect sl d  -- minimal progress to consider
              in n `div` k
        pe = let (n, k) = intersect el d   -- maximal progress to consider
               -- Corners obstruct view, so the steep line, constructed
               -- from corners, is itself not a part of the view,
               -- so if its intersection with the line of diagonals is only
               -- at a corner, choose the diamond leading to a smaller view.
             in -1 + n `divUp` k
        inside = [B(p, d) | p <- [ps0..pe]]
        outside
          | d >= r = []
          | isClear (B(ps0, d)) = mscan (Just s0) (ps0+1) pe  -- start in light
          | otherwise = mscan Nothing (ps0+1) pe              -- start in shadow
    in assert (r >= d && d >= 0 && pe >= ps0 `blame` (r,d,s0,e,ps0,pe)) $
       inside ++ outside
   where
    -- The current state of a scan is kept in @Maybe Edge@.
    -- If it's the @Just@ case, we're in a visible interval. If @Nothing@,
    -- we're in a shadowed interval.
    mscan :: Maybe Edge -> Progress -> Progress -> [Bump]
    mscan (Just s@(_, sBumps)) ps pe
      | ps > pe = dscan (d+1) (s, e)          -- reached end, scan next
      | not $ isClear steepBump =             -- entering shadow
          mscan Nothing (ps+1) pe
          ++ dscan (d+1) (s, (dline nep steepBump, neBumps))
      | otherwise = mscan (Just s) (ps+1) pe  -- continue in light
     where
      steepBump = B(ps, d)
      gte = dsteeper steepBump
      nep = maximal gte sBumps
      neBumps = addHull gte steepBump eBumps

    mscan Nothing ps pe
      | ps > pe = []                          -- reached end while in shadow
      | isClear shallowBump =                 -- moving out of shadow
          mscan (Just (dline nsp shallowBump, nsBumps)) (ps+1) pe
      | otherwise = mscan Nothing (ps+1) pe   -- continue in shadow
     where
      shallowBump = B(ps, d)
      gte = flip $ dsteeper shallowBump
      nsp = maximal gte eBumps
      nsBumps = addHull gte shallowBump sBumps0

-- | Create a line from two points. Debug: check if well-defined.
dline :: Bump -> Bump -> Line
dline p1 p2 =
  assert (uncurry blame $ debugLine (p1, p2)) $
  (p1, p2)

-- | Compare steepness of @(p1, f)@ and @(p2, f)@.
-- Debug: Verify that the results of 2 independent checks are equal.
dsteeper :: Bump ->  Bump -> Bump -> Bool
dsteeper f p1 p2 =
  assert (res == debugSteeper f p1 p2) $
  res
   where res = steeper f p1 p2

-- | The X coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of diamonds at distance
-- @d@ from (0, 0).
intersect :: Line -> Distance -> (Int, Int)
intersect (B(x, y), B(xf, yf)) d =
  assert (allB (>= 0) [y, yf]) $
  ((d - y)*(xf - x) + x*(yf - y), yf - y)
{-
Derivation of the formula:
The intersection point (xt, yt) satisfies the following equalities:
yt = d
(yt - y) (xf - x) = (xt - x) (yf - y)
hence
(yt - y) (xf - x) = (xt - x) (yf - y)
(d - y) (xf - x) = (xt - x) (yf - y)
(d - y) (xf - x) + x (yf - y) = xt (yf - y)
xt = ((d - y) (xf - x) + x (yf - y)) / (yf - y)

General remarks:
A diamond is denoted by its left corner. Hero at (0, 0).
Order of processing in the first quadrant rotated by 45 degrees is
 45678
  123
   @
so the first processed diamond is at (-1, 1). The order is similar
as for the restrictive shadow casting algorithm and reversed wrt PFOV.
The line in the curent state of mscan is called the shallow line,
but it's the one that delimits the view from the left, while the steep
line is on the right, opposite to PFOV. We start scanning from the left.

The Loc coordinates are cartesian. The Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and rotated so that he always
looks at the first (rotated 45 degrees) quadrant. The (Progress, Distance)
cordinates coincide with the Bump coordinates, unlike in PFOV.
-}

-- | Debug functions for DFOV:

-- | Debug: calculate steeper for DFOV in another way and compare results.
debugSteeper :: Bump -> Bump -> Bump -> Bool
debugSteeper f@(B(_xf, yf)) p1@(B(_x1, y1)) p2@(B(_x2, y2)) =
  assert (allB (>= 0) [yf, y1, y2]) $
  let (n1, k1) = intersect (p1, f) 0
      (n2, k2) = intersect (p2, f) 0
  in n1 * k2 >= k1 * n2

-- | Debug: check is a view border line for DFOV is legal.
debugLine :: Line -> (Bool, String)
debugLine line@(B(x1, y1), B(x2, y2))
  | not (allB (>= 0) [y1, y2]) =
      (False, "negative coordinates: " ++ show line)
  | y1 == y2 && x1 == x2 =
      (False, "ill-defined line: " ++ show line)
  | y1 == y2 =
      (False, "horizontal line: " ++ show line)
  | crossL0 =
      (False, "crosses the X axis below 0: " ++ show line)
  | crossG1 =
      (False, "crosses the X axis above 1: " ++ show line)
  | otherwise = (True, "")
 where
  (n, k)  = intersect line 0
  (q, r)  = if k == 0 then (0, 0) else n `divMod` k
  crossL0 = q < 0  -- q truncated toward negative infinity
  crossG1 = q >= 1 && (q > 1 || r /= 0)
