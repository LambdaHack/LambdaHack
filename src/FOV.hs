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
type Line         = (Bump, Bump)
type ConvexHull   = [Bump]
type Edge         = (Line, ConvexHull)
type EdgeInterval = (Edge, Edge)

data FovMode = Shadow | Permissive Int | Diagonal Int

-- | Perform a full scan for a given location. Returns the locations
-- that are currently in the field of view.
fullscan :: FovMode -> Loc -> LMap -> Set Loc
fullscan fovMode loc lmap =
  case fovMode of
    Shadow ->         -- shadow casting with infinite range
      S.unions $
      L.map (\ tr ->
              scan (tr loc) lmap 1 (0,1)) -- was: scan (tr loc) lmap 0 (0,1); TODO: figure out what difference this makes
      [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Permissive r  ->  -- precise permissive with range r
      S.unions $
      L.map (\ tr ->
              pscan r (tr loc) lmap 1
                (((B(1, 0), B(0, r+1)), [B(0, 1)]),
                 ((B(0, 1), B(r+1, 0)), [B(1, 0)])))
      [qtr0,qtr1,qtr2,qtr3]
    Diagonal r    ->  -- diagonal with range r
      S.unions $
      L.map (\ tr ->
              dscan r (tr loc) lmap 1
                (((B(0, 1), B(r, -r)),  [B(0, 0)]),
                 ((B(0, 0), B(r, r+1)), [B(0, 1)])))
      [qtr0,qtr1,qtr2,qtr3]


-- |Common functions:

-- | The translation, rotation and symmetry functions for octants.
tr0 (oy,ox) (d,p) = (oy + d,ox + p)
tr1 (oy,ox) (d,p) = (oy + d,ox - p)
tr2 (oy,ox) (d,p) = (oy - d,ox + p)
tr3 (oy,ox) (d,p) = (oy - d,ox - p)
tr4 (oy,ox) (d,p) = (oy + p,ox + d)
tr5 (oy,ox) (d,p) = (oy + p,ox - d)
tr6 (oy,ox) (d,p) = (oy - p,ox + d)
tr7 (oy,ox) (d,p) = (oy - p,ox - d)

-- | The translation and rotation functions for quadrants.
qtr0, qtr1, qtr2, qtr3 :: Loc -> Bump -> Loc
qtr0 (oy, ox) (B(y, x)) = (oy - y, ox + x)  -- first quadrant
qtr1 (oy, ox) (B(y, x)) = (oy - x, ox - y)  -- then rotated clockwise 90 degrees
qtr2 (oy, ox) (B(y, x)) = (oy + y, ox - x)
qtr3 (oy, ox) (B(y, x)) = (oy + x, ox + y)

-- | Integer division, rounding up.
divUp n k = - (-n) `div` k

-- | Maximal element of a non-empty list. Prefers elements from the rear,
-- which is essential for PFOV, to avoid ill-defined lines.
maximal :: (a -> a -> Bool) -> [a] -> a
maximal gte = L.foldl1' (\ acc x -> if gte x acc then x else acc)

-- | Check if the line from the second point to the first is more steep
-- than the line from the third point to the first. This is related
-- to the formal notion of gradient (or angle), but hacked wrt signs
-- to work in this particular setup. Returns True for ill-defined lines.
steeper :: Bump ->  Bump -> Bump -> Bool
steeper (B(yf, xf)) (B(y1, x1)) (B(y2, x2)) =
  (yf - y1)*(xf - x2) >= (yf - y2)*(xf - x1)

-- | Adds a bump to the convex hull of bumps represented as a list.
addHull :: (Bump -> Bump -> Bool) -> Bump -> ConvexHull -> ConvexHull
addHull gte b l =
  case l of
    x:y:zs ->
      if gte x y
      then addHull gte b (y:zs)
      else b : l
    _ -> b : l


-- | A restrictive variant of Recursive Shadow Casting FOV with infinite range.
-- It's not designed for dungeons with diagonal walls, so they block visibility,
-- though they don't block movement. Such cases appear in LambdaHack only
-- when two corridors touch diagonally by accident and on the random pillars
-- levels.

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

downBias, upBias :: (Integral a, Integral b) => Ratio a -> b
downBias x = round (x - 1 % (denominator x * 3))
upBias   x = round (x + 1 % (denominator x * 3))


-- | Precise Permissive FOV with a given range.
-- Clean-room reimplemented based on the algorithm described in http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View, though the general structure is more influenced by recursive shadow casting, as implemented above. In the result, this algorithm is much faster on dense maps, since it does not scan areas blocked by shadows. See https://github.com/Mikolaj/LambdaHack/wiki/Fov-and-los for some more context.

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
        | open (l `at` tr (d, ps)) = pscan' (Just s) ps  -- start in light
        | ps == ns `divUp` ks = pscan' (Just s) ps       -- start in a corner
        | otherwise = pscan' Nothing (ps+1)              -- start in mid-wall

      dp2bump     (d, p) = B(p, d - p)
      bottomRight (d, p) = B(p, d - p + 1)
      tr = ptr . dp2bump

      pscan' :: Maybe Edge -> Progress -> Set Loc
      pscan' (Just s@(_, sBumps)) ps
        | ps > pe =                            -- reached end, scan next
            pscan r ptr l (d+1) (s, e)
        | closed (l `at` tr (d, ps)) =         -- entering shadow, steep bump
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

-- | Debug functions for DFOV:

-- | Debug: calculate steeper for DFOV in another way and compare results.
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


-- | Digital FOV with a given range.
-- Specification is at http://roguebasin.roguelikedevelopment.org/index.php?title=Digital_field_of_view_implementation, but AFAIK, this algorithm (fast DFOV done similarly as PFOV) has never been implemented before. The algorithm is based on the PFOV algorithm, clean-room reimplemented based on http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View. See https://github.com/Mikolaj/LambdaHack/wiki/Fov-and-los for some more context.

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
