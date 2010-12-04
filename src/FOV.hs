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
type Tr       = (Distance, Progress) -> Loc
type Line         = (Loc, Loc)
type LineInterval = (Line, Line)
data Bump         = Bump (Distance, Progress)
type ConvexHull   = [Bump]
type HullInterval = (ConvexHull, ConvexHull)
data WhichLine    = Steep | Shallow
  deriving (Show, Eq)

-- | Recursive Shadow Casting FOV with infinite visibility range.
-- It's not tuned enough for special cases, such as visibility through
-- a diagonal line of walls (this appears in LambdaHack only
-- when two corridors touch diagonally by accident).

-- | The current state of a scan is kept in a variable of Maybe Rational.
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
scan :: Tr -> LMap -> Distance -> Interval -> Set Loc
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
              scan (tr loc) lmap 0 (0,1))
      [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Just n  ->  -- precise permissive with range n
      S.unions $
      L.map (\ tr ->
              pscan n (tr loc) lmap 0
                (((1, 0), (0, 2*n)), ((0, 1), (2*n, 0))) ([], []))
      [ptr0,ptr1,ptr2,ptr3]


-- | Precise Permissive FOV with a given range (TODO).
-- Clean-room reimplemented based on http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View

-- | The current state of a scan is kept in Maybe (Line, ConvexHull).
-- If Just something, we're in a visible interval. If Nothing, we're in
-- a shadowed interval.
pscan :: Distance -> Tr -> LMap ->
         Distance -> LineInterval -> HullInterval -> Set Loc
pscan n tr l d (s{-shallow line-}, e{-steep line-}) (sBumps, eBumps) =
  let ps    = intersectD Shallow s d  -- minimal progress to check
      pe    = intersectD Steep e d    -- maximal progress to check
      start = if open (l `at` tr (d, ps))
              then Just (s, sBumps)   -- start in light
              else Nothing            -- start in shadow
  in
   -- trace (show (d,s,e,ps,pe)) $
   S.union
     (S.fromList [tr (d, p) | p <- [ps..pe]])
     (if d == n then S.empty else pscan' start ps pe)
     -- the area is diagonal, which is wrong; for Digital FOV it'd be a square
     where
       pscan' :: Maybe (Line, ConvexHull) -> Progress -> Progress -> Set Loc
       -- pscan' start ps pe
       --   | trace (show (start,ps,pe)) False = undefined
       pscan' (Just (s, sBumps)) ps pe
         | s `cornerEq` e = S.empty        -- illegal one-line view
         | ps > pe =                       -- reached end, scan next
             pscan n tr l (d+1) (s, e) (sBumps, eBumps)
         | closed (l `at` tr (d, ps)) =    -- entering shadow, steep bump
             fromMaybe S.empty
               (do
                  let steepBump = Bump (d, ps)
                  ne <- lineOfHull tr Steep steepBump sBumps -- can fail?
                  let neBumps = addHull steepBump eBumps
                  return $
                    S.union
                      (pscan n tr l (d+1) (s, ne) (sBumps, neBumps))
                      (pscan' Nothing (ps+1) pe))
         | otherwise =                     -- continue in light
             pscan' (Just (s, sBumps)) (ps+1) pe

       pscan' Nothing ps pe
         | ps > pe = S.empty               -- reached end while in shadow
         | open (l `at` tr (d, ps)) =      -- moving out of shadow, shallow bump
             fromMaybe S.empty
               (do
                  let shallowBump = Bump (d, ps)
                  ns <- lineOfHull tr Shallow shallowBump eBumps  -- can fail
                  let nsBumps = [shallowBump]
                  return $ pscan' (Just (ns, nsBumps)) (ps+1) pe)
         | open (l `at` tr (d+1, ps+1)) =  -- diagonal view through a corner
             fromMaybe S.empty
               (do
                  let shallowBump = Bump (d, ps)
                      steepBump   = Bump (d, ps+1)
                      nsBumps = [shallowBump]
                      neBumps = [steepBump]
                  ns <- lineOfHull tr Shallow shallowBump neBumps  -- can fail?
                  ne <- lineOfHull tr Steep steepBump nsBumps      -- can fail?
                  return $
                    S.union
                      (pscan n tr l (d+1) (ns, ne) (nsBumps, neBumps))
                      (pscan' Nothing (ps+1) pe))
         | otherwise =                     -- continue in shadow
             pscan' Nothing (ps+1) pe

{- THE Y COORDINATE COMES FIRST! (Y,X)!
Bottom-left corner denotes a tile square. Hero at (0,0). Order of processing
in the first quadrant is
 9
 58
 247
1@136
 2
so the first processed square is at (0, 1). The order is reversed
wrt the shadow casting algorithm above. The line in the curent state
of the scan is not the steep line, but the shallow line, the one from which
we start going from the bottom right.
-}
ptr0 (oy,ox) (d,p) = (oy + p, ox + d - p)  -- first quadrant
ptr1 (oy,ox) (d,p) = (oy - p, ox + d - p)  -- then clockwise
ptr2 (oy,ox) (d,p) = (oy - p, ox - d + p)
ptr3 (oy,ox) (d,p) = (oy + p, ox - d + p)

--TODO:
--the functions below are wrong, because I'm mixing the cartesian
--coordinates Loc the special coordinates Bump, given by ptr*

-- | The progress at which a line intersects the diagonal set of tiles
-- at the given distance.
intersectD :: WhichLine -> Line -> Distance -> Progress
intersectD which ((y1, x1), (y2, x2)) d =
  -- The diagonals of the tiles lie on the line y = 1 + d - x,
  -- hence the intersection point (y0, x0) has the following y0 coordinate:
  let y0 = ((1 + d) * (y2 - y1) + y1 * x2 - x1 * y2) % ((x2 - x1) + (y2 - y1))
      yr = round y0
  in
   -- Corners do not block view, so if the line interects the very
   -- corner of 2 tiles, choose the tile that provides a larger view.
   if denominator y0 == 1 && y0 >= 1 && which == Shallow then yr - 1 else yr
{-
y = d - x
y - y1 = (x - x1) (y2 - y1) / (x2 - x1)

y (x2 - x1) - y1 x2 = x (y2 - y1) - x1 y2

y (x2 - x1) - y1 x2 = d (y2 - y1) - y (y2 - y1) - x1 y2

y (x2 - x1) + y (y2 - y1) = d (y2 - y1) - x1 y2 + y1 x2

y = (d (y2 - y1) + y1 x2 - x1 y2) / (x2 - x1 + y2 - y1)
-}

-- | Checks if the shallow and steep lines are equal,
-- but only if both come from the same extreme corner of hero's tile.
-- If the two lines are equal, the view is a single line.
-- Since the hero's extreme corners do not count for visibility,
-- a single line view originating from them is canceled,
-- after beging detected with this function.
cornerEq :: Line -> Line -> Bool
cornerEq ((1, 0), (ys, xs)) ((1, 0), (ye, xe)) = ys * xe == ye * xs
cornerEq ((0, 1), (ys, xs)) ((0, 1), (ye, xe)) = ys * xe == ye * xs
cornerEq _ _ = False

-- | Constructs steep or shallow line from the far point and the opposite
-- convex hull of bumps. Fails if the bumps force the line
-- out of the hero's tile. TODO: other special cases? remove these?
lineOfHullRaw :: Tr -> WhichLine -> Bump -> ConvexHull -> Maybe Line
lineOfHullRaw tr which (Bump (1, 0)) [Bump (1, 1)] =
  Just ((1, 1), (1, 2))  -- diagonal view through a corner
lineOfHullRaw tr which (Bump (1, 1)) [Bump (1, 0)] =
  Just ((1, 1), (2, 1))
lineOfHullRaw tr which (Bump (d, ps)) hull =
  case which of
    Shallow -> Just ((1, 0), topLeft     (d, ps))  -- TODO
    Steep   -> Just ((0, 1), bottomRight (d, ps))  -- TODO
    where
      topLeft     (d, ps) = tr (d + 1, ps + 1)
      bottomRight (d, ps) = tr (d + 1, ps)

lineOfHull :: Tr -> WhichLine -> Bump -> ConvexHull -> Maybe Line
lineOfHull tr which bump hull =
  case lineOfHullRaw tr which bump hull of
    Just ((y1, x1), (y2, x2))
      | y1 == y2 && x1 == x2 -> error "lineOfHull: invalid line"
    result -> result

-- | Adds a bump to a the convex hull of bumps represented as a list.
-- TODO: Can be optimizaed by removing some points from the list.
addHull :: Bump -> ConvexHull -> ConvexHull
addHull loc l = loc : l



-- ########
-- ###?.?##
-- ####.?##
-- #...@###
-- #######5
-- ######424
-- #####31@13
--       424
--        5

-- I think the line can always come from the diagonal, because it has to cross the diagonal, so it can be represented as a rational from 0 to 1; but perhaps instead represent it as crossing a point at the other side of the hero, if it's more natural; I think integer points would be enough, hmm, points in the same quadrant should be enough, too.
-- I think we really need to keep the list of previous shallow and steep bumps, but e.g. for the steep line, it's enough to keep a convex hull of (bumps, the leftmost end of the diagonal and the infinity point). Many points can be dropped without changing the hull.
-- This is false: [So the steep line is one of the lines of the convex hull that crosses diagonal as far to the right as possible or it passes through one point of the hull and the rightmost point of the diagonal. No other option. Either case, it's integer coordinates.]
-- What is enough is a steep bump and one point from the shallow convex hull.
-- Each tile adds 1 point to the convex hull. 2 is not needed, because the map is to coarse for a point 1 unit away at the right angle to make a difference.
-- This is not quite true, either: [The initial value of lSteep may be [(0,1), (0, n)]. So the steep line is calculated to be from (0, n) to (1, 0). For a start, I may always compute the line line from the hull in pscan, because it does not change in pscan, because the lists don't change.]
