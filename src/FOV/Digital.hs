module FOV.Digital where

import Data.Set as S

import FOV.Common
import Geometry
import Level

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
dscan :: Distance -> (Bump -> Loc) -> LMap -> Distance -> EdgeInterval ->
         Set Loc
dscan r tr l d (s@(sl{-shallow line-}, sBumps), e@(el{-steep line-}, eBumps)) =
  -- trace (show (d,s,e,ps,pe)) $
  S.union outside (S.fromList [tr (B(d, p)) | p <- [ps..pe]])
    -- the scanned area is a square, which is a sphere in this metric; good
    where
      ps = let (n, k) = dintersect sl d       -- minimal progress to check
           in  n `div` k
      pe = let (n, k) = dintersect el d       -- maximal progress to check
               -- Corners obstruct view, so the steep line, constructed
               -- from corners, is itself not a part of the view,
               -- so if its intersection with the line of diagonals is only
               -- at a corner, choose the diamond leading to a smaller view.
           in  -1 + n `divUp` k
      outside
        | d >= r = S.empty
        | ps > pe = error $ "dscan: wrong start " ++ show (d, (ps, pe))
        | open (l `at` tr (B(d, ps))) =
            dscan' (Just s) (ps+1)            -- start in light, jump ahead
        | otherwise = dscan' Nothing (ps+1)   -- start in shadow, jump ahead

      dscan' :: Maybe Edge -> Progress -> Set Loc
      dscan' (Just s@(_, sBumps)) ps
        | ps > pe = dscan r tr l (d+1) (s, e) -- reached end, scan next
        | closed (l `at` tr steepBump) =      -- entering shadow
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
        | ps > pe = S.empty                   -- reached end while in shadow
        | open (l `at` tr shallowBump) =      -- moving out of shadow
            dscan' (Just (dline nsp shallowBump, nsBumps)) (ps+1)
        | otherwise = dscan' Nothing (ps+1)   -- continue in shadow
        where
          shallowBump = B(d, ps)
          gte = flip $ dsteeper shallowBump
          nsp = maximal gte eBumps
          nsBumps = addHull gte shallowBump sBumps

      dline p1 p2 =
        ddebugLine $  -- TODO: disable when it becomes a bottleneck
        (p1, p2)

      dsteeper f p1 p2 =
        ddebugSteeper f p1 p2 $  -- TODO: disable when it becomes a bottleneck
        steeper f p1 p2

-- | The x coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of diamonds at distance d from (0, 0).
dintersect :: Line -> Distance -> (Int, Int)
dintersect (B(y, x), B(yf, xf)) d =
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
as for the shadow casting algorithm above and reversed wrt PFOV.
The line in the curent state of scan' is called the shallow line,
but it's the one that delimits the view from the left, while the steep
line is on the right, opposite to PFOV. We start scanning from the left.

The Loc coordinates are cartesian, the Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and rotated so that he always
looks at the first roatated quadrant, the (Distance, Progress) cordinates
coincide with the Bump coordinates, unlike in PFOV.
-}

-- | Debug functions for DFOV:

-- | Debug: calculate steeper for DFOV in another way and compare results.
ddebugSteeper :: Bump ->  Bump -> Bump -> Bool -> Bool
ddebugSteeper f p1 p2 x =
  let (n1, k1) = dintersect (p1, f) 0
      (n2, k2) = dintersect (p2, f) 0
  in  if x == (n1 * k2 >= k1 * n2)
      then x
      else error $ "dsteeper: " ++ show (f, p1, p2, x)

-- | Debug: check is a view border line for DFOV is legal.
ddebugLine :: Line -> Line
ddebugLine line@(B(y1, x1), B(y2, x2))
  | y1 == y2 && x1 == x2 =
      error $ "ddebugLine: wrongly defined line " ++ show line
  | y2 - y1 == 0 =
      error $ "ddebugLine: horizontal line " ++ show line
  | crossL0 =
      error $ "ddebugLine: crosses the X axis below 0 " ++ show line
  | crossG1 =
      error $ "ddebugLine: crosses the X axis above 1 " ++ show line
  | otherwise = line
    where
      (n, k)  = dintersect line 0
      (q, r)  = if k == 0 then (0, 0) else n `divMod` k
      crossL0 = q < 0  -- q truncated toward negative infinity
      crossG1 = q >= 1 && (q > 1 || r /= 0)
