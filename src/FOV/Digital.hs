module FOV.Digital (dscan) where

import qualified Data.Set as S

import Assert
import FOV.Common
import Geometry
import Level
import qualified Tile

-- Digital FOV with a given range.

-- | DFOV, according to specification at http://roguebasin.roguelikedevelopment.org/index.php?title=Digital_field_of_view_implementation,
-- but AFAIK, this algorithm (fast DFOV done similarly as PFOV) has never been
-- implemented before. The algorithm is based on the PFOV algorithm,
-- clean-room reimplemented based on http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View.
-- See https://github.com/Mikolaj/LambdaHack/wiki/Fov-and-los
-- for some more context.

-- | The current state of a scan is kept in Maybe (Line, ConvexHull).
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
dscan :: Distance -> (Bump -> Loc) -> LMap -> Distance -> EdgeInterval
         -> S.Set Loc
dscan r tr l d (s0@(sl{-shallow line-}, sBumps0), e@(el{-steep line-}, eBumps))=
  assert (pe >= ps0 && r >= d && d >= 0 `blame` (r,d,s0,e,ps0,pe)) $
  S.union outside (S.fromList [tr (B(d, p)) | p <- [ps0..pe]])
    -- the scanned area is a square, which is a sphere in this metric; good
    where
      ps0 = let (n, k) = dintersect sl d      -- minimal progress to check
            in  n `div` k
      pe = let (n, k) = dintersect el d       -- maximal progress to check
               -- Corners obstruct view, so the steep line, constructed
               -- from corners, is itself not a part of the view,
               -- so if its intersection with the line of diagonals is only
               -- at a corner, choose the diamond leading to a smaller view.
           in  -1 + n `divUp` k
      outside
        | d >= r = S.empty
        | Tile.isClear (l `at` tr (B(d, ps0))) =
            dscan' (Just s0) (ps0+1)          -- start in light, jump ahead
        | otherwise = dscan' Nothing (ps0+1)  -- start in shadow, jump ahead

      dscan' :: Maybe Edge -> Progress -> S.Set Loc
      dscan' (Just s@(_, sBumps)) ps
        | ps > pe = dscan r tr l (d+1) (s, e) -- reached end, scan next
        | not $ Tile.isClear (l `at` tr steepBump) = -- entering shadow
            S.union
              (dscan r tr l (d+1) (s, (dline nep steepBump, neBumps)))
              (dscan' Nothing (ps+1))
        | otherwise = dscan' (Just s) (ps+1)  -- continue in light
        where
          steepBump = B(d, ps)
          gte = dsteeper steepBump
          nep = maximal gte sBumps
          neBumps = addHull gte steepBump eBumps

      dscan' Nothing ps
        | ps > pe = S.empty                      -- reached end while in shadow
        | Tile.isClear (l `at` tr shallowBump) = -- moving out of shadow
            dscan' (Just (dline nsp shallowBump, nsBumps)) (ps+1)
        | otherwise = dscan' Nothing (ps+1)      -- continue in shadow
        where
          shallowBump = B(d, ps)
          gte = flip $ dsteeper shallowBump
          nsp = maximal gte eBumps
          nsBumps = addHull gte shallowBump sBumps0

      dline p1 p2 =
        assert (uncurry blame $ ddebugLine (p1, p2)) $
        (p1, p2)

      dsteeper f p1 p2 =
        assert (res == ddebugSteeper f p1 p2) $
        res
          where res = steeper f p1 p2

-- | The x coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of diamonds at distance d from (0, 0).
dintersect :: Line -> Distance -> (Int, Int)
dintersect (B(y, x), B(yf, xf)) d =
  assert (allB (>= 0) [y, yf]) $
  ((d - y)*(xf - x) + x*(yf - y), yf - y)
{-
Derivation of the formula:
The intersection point (yt, xt) satisfies the following equalities:
yt = d
(yt - y) (xf - x) = (xt - x) (yf - y)
hence
(yt - y) (xf - x) = (xt - x) (yf - y)
(d - y) (xf - x) = (xt - x) (yf - y)
(d - y) (xf - x) + x (yf - y) = xt (yf - y)
xt = ((d - y) (xf - x) + x (yf - y)) / (yf - y)

General remarks:
A diamond is denoted by its left corner. Hero at (0,0).
Order of processing in the first quadrant rotated by 45 degrees is
 45678
  123
   @
so the first processed diamond is at (-1, 1). The order is similar
as for the restrictive shadow casting algorithm and reversed wrt PFOV.
The line in the curent state of scan' is called the shallow line,
but it's the one that delimits the view from the left, while the steep
line is on the right, opposite to PFOV. We start scanning from the left.

The Loc coordinates are cartesian. The Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and rotated so that he always
looks at the first (rotated 45 degrees) quadrant. The (Distance, Progress)
cordinates coincide with the Bump coordinates, unlike in PFOV.
-}

-- | Debug functions for DFOV:

-- | Debug: calculate steeper for DFOV in another way and compare results.
ddebugSteeper :: Bump -> Bump -> Bump -> Bool
ddebugSteeper f@(B(yf, _xf)) p1@(B(y1, _x1)) p2@(B(y2, _x2)) =
  assert (allB (>= 0) [yf, y1, y2]) $
  let (n1, k1) = dintersect (p1, f) 0
      (n2, k2) = dintersect (p2, f) 0
  in n1 * k2 >= k1 * n2

-- | Debug: check is a view border line for DFOV is legal.
ddebugLine :: Line -> (Bool, String)
ddebugLine line@(B(y1, x1), B(y2, x2))
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
      (n, k)  = dintersect line 0
      (q, r)  = if k == 0 then (0, 0) else n `divMod` k
      crossL0 = q < 0  -- q truncated toward negative infinity
      crossG1 = q >= 1 && (q > 1 || r /= 0)
