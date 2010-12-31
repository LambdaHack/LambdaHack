module FOV where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Ratio
import Data.Maybe
import Debug.Trace

import Geometry
import Level

type Interval = (Rational, Rational)
type Distance = Int
type Progress = Int
newtype Bump      = B Loc  deriving (Show)
type ConvexHull   = [Bump]
type HullInterval = (ConvexHull, ConvexHull)
type Line         = (Bump, Bump)
type LineInterval = (Line, Line)
data WhichLine    = Shallow | Steep  deriving (Show, Eq)

-- | Recursive Shadow Casting FOV with infinite visibility range.
-- It's not tuned enough for special cases, such as visibility through
-- a diagonal line of walls (this appears in LambdaHack only
-- when two corridors touch diagonally by accident).

-- | The current state of a scan is kept in a variable of Maybe Rational.
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
scan :: ((Distance, Progress) -> Loc) -> LMap -> Distance -> Interval -> Set Loc
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
              scan (tr loc) lmap 1 (0,1)) -- was: scan (tr loc) lmap 0 (0,1); TODO: figure out what difference this makes
      [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Just n  ->  -- precise permissive with range n
      S.unions $
      L.map (\ tr ->
              pscan n (tr loc) lmap 1
                ((B(1, 0), B(0, 2*n)), (B(0, 1), B(2*n, 0))) ([], []))
      [ptr0,ptr1,ptr2,ptr3]


-- | Precise Permissive FOV with a given range (TODO).
-- Clean-room reimplemented based on http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View

-- | The current state of a scan is kept in Maybe (Line, ConvexHull).
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
pscan :: Distance -> (Bump -> Loc) -> LMap ->
         Distance -> LineInterval -> HullInterval -> Set Loc
pscan n ptr l d (s{-shallow line-}, e{-steep line-}) (sBumps, eBumps) =
  let ps    = let (n, k) = intersectD s d
              in  n `quot` k                   -- minimal progress to check
      pe    = let (n, k) = intersectD e d
                  (q, r) = n `quotRem` k
                  -- Corners are translucent, so they are invisible,
                  -- so if intersection is at a corner, choose the square
                  -- that creates the smaller view.
              in  if r == 0 then q - 1 else q  -- maximal progress to check
      start = if open (l `at` tr (d, ps))
              then Just (s, sBumps)  -- start in light
              else Nothing           -- start in shadow
  in
   -- trace (show (d,s,e,ps,pe)) $
   S.union
     (S.fromList [tr (d, p) | p <- [ps..pe]])
     (if d == n then S.empty else pscan' start ps pe)
     -- the area is diagonal, which is wrong; for Digital FOV it'd be a square
     where
       dp2bump     (d, p) = B(p, d - p)
       topLeft     (d, p) = B(p + 1, d - p)
       bottomRight (d, p) = B(p, d - p + 1)
       tr = ptr . dp2bump

       pscan' :: Maybe (Line, ConvexHull) -> Progress -> Progress -> Set Loc
       pscan' (Just (s, sBumps)) ps pe
         | ps > pe =                       -- reached end, scan next
             pscan n ptr l (d+1) (s, e) (sBumps, eBumps)
         | closed (l `at` tr (d, ps)) =    -- entering shadow, steep bump
             let steepBump = bottomRight (d, ps)
                 ne = borderLine Steep steepBump sBumps
                 neBumps = addHull steepBump eBumps
             in  S.union
                   (pscan n ptr l (d+1) (s, ne) (sBumps, neBumps))
                   (pscan' Nothing ps pe)
         | otherwise =                     -- continue in light
             pscan' (Just (s, sBumps)) (ps+1) pe

       pscan' Nothing ps pe
         | ps > pe = S.empty               -- reached end while in shadow
         | open (l `at` tr (d, ps)) =      -- moving out of shadow, shallow bump
             let shallowBump = bottomRight (d, ps)
                 ns = borderLine Shallow shallowBump eBumps
                 nsBumps = addHull shallowBump sBumps
             in  pscan' (Just (ns, nsBumps)) (ps+1) pe
         | ps+1 > pe = S.empty             -- nothing to do up-left
         | closed (l `at` tr (d, ps+1)) =  -- up-left is in shadow, too
             -- This is a special case. Corners are translucent, so they
             -- do not block view, so a square blocked by diagonal walls
             -- is still visible through their common corner.
             if closed (l `at` tr (d+1, ps+1))
             then
               -- trace (show ("wall visible through its corner",ps,pe)) $
               S.insert (tr (d+1, ps+1))
                 (pscan' Nothing (ps+1) pe)
             else
               -- trace (show ("open space visible through its corner",ps,pe)) $
               let bump    = topLeft (d, ps)
                   nsBumps = addHull bump sBumps
                   neBumps = addHull bump eBumps
                   ns = borderLine Shallow bump neBumps
                   ne = borderLine Steep bump nsBumps
               in  S.union
                     (pscan n ptr l (d+1) (ns, ne) (nsBumps, neBumps))
                     (pscan' Nothing (ps+1) pe)
         | otherwise =                     -- continue in shadow
             pscan' Nothing (ps+1) pe

{- THE Y COORDINATE COMES FIRST! (Y,X)!
Bottom-left corner denotes a square. Hero at (0,0). Order of processing
in the first quadrant is
9
58
247
@136
so the first processed square is at (0, 1). The order is reversed
wrt the shadow casting algorithm above. The line in the curent state
of the scan is not the steep line, but the shallow line, the one from which
we start going from the bottom right.

The Loc coordinates are cartesian, the Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and always looks at the first quadrant,
the (Distance, Progress) cordinates are mangled and not used for geometry.
-}
ptr0, ptr1, ptr2, ptr3 :: Loc -> Bump -> Loc
ptr0 (oy, ox) (B(y, x)) = (oy - y, ox + x)  -- first quadrant
ptr1 (oy, ox) (B(y, x)) = (oy - x, ox - y)  -- then rotated clockwise 90 degrees
ptr2 (oy, ox) (B(y, x)) = (oy + y, ox - x)
ptr3 (oy, ox) (B(y, x)) = (oy + x, ox + y)

-- | The y coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of squares at distance d from (0, 0).
intersectD :: Line -> Distance -> (Int, Int)
intersectD (B(y, x), B(yf, xf)) d =
  ((1 + d)*(yf - y) + y*xf - x*yf, (xf - x) + (yf - y))
{-
The intersection point (yt, xt) satisfies the following equalities:
xt = 1 + d - yt
(yt - y) (xf - x) = (xt - x) (yf - y)
hence
(yt - y) (xf - x) = (xt - x) (yf - y)
yt (xf - x) - y xf = xt (yf - y) - x yf
yt (xf - x) - y xf = (1 + d) (yf - y) - yt (yf - y) - x yf
yt (xf - x) + yt (yf - y) = (1 + d) (yf - y) - x yf + y xf
yt = ((1 + d) (yf - y) + y xf - x yf) / (xf - x + yf - y)
-}

-- | Constructs steep or shallow line from the far point and the opposite
-- convex hull of bumps.
borderLine :: WhichLine -> Bump -> ConvexHull -> Line
borderLine which farPoint hull =
  let crossLeq (n1, k1) (n2, k2) = n1 * k2 <= k1 * n2
      (extraBump, strongerBump) =
        case which of
          Shallow -> ((B(1, 0), (1, 1)), crossLeq)
          Steep   -> ((B(0, 1), (0, 1)), \ a b -> crossLeq b a)
      cross acc@(_, nkAcc) bump =
        let nkNew = intersectD (bump, farPoint) 0
        in if strongerBump nkAcc nkNew
           then acc
           else (bump, nkNew)
      (strongestBump, _) = L.foldl' cross extraBump hull
      line =
        -- trace (show (which, strongestBump, farPoint, hull)) $
        checkBorderLine $
        (strongestBump, farPoint)
  in  line

-- | Checks postconditions of borderLine.
checkBorderLine :: Line -> Line
checkBorderLine line@(B(y1, x1), B(y2, x2))
  | y1 == y2 && x1 == x2 =
      error $ "borderLine: wrongly defined line " ++ show line
  | x2 - x1 == - (y2 - y1) =
      error $ "borderLine: diagonal line " ++ show line
  | crossL0 =
      error $ "borderLine: crosses diagonal below 0 " ++ show line
  | crossG1 =
      error $ "borderLine: crosses diagonal above 1 " ++ show line
  | otherwise = line
    where
      (n, k) = intersectD line 0
      crossL0 = n * k < 0
      crossG1 = n * k > k * k

-- | Adds a bump to the convex hull of bumps represented as a list.
-- TODO: Can be optimized by removing some points from the list,
-- knowing that (1, 0) or (0, 1) belong to the hull, too.
addHull :: Bump -> ConvexHull -> ConvexHull
addHull loc l = loc : l
