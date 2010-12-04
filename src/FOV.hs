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

newtype Bump      = B Loc
type ConvexHull   = [Bump]
type HullInterval = (ConvexHull, ConvexHull)
type Line         = (Bump, Bump)
type LineInterval = (Line, Line)
data WhichLine    = Steep | Shallow
  deriving (Show, Eq)

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
              scan (tr loc) lmap 0 (0,1))
      [tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7]
    Just n  ->  -- precise permissive with range n
      S.unions $
      L.map (\ tr ->
              pscan n (tr loc) lmap 0
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
       tr = ptr . dp2bump
       pscan' :: Maybe (Line, ConvexHull) -> Progress -> Progress -> Set Loc
       -- pscan' start ps pe
       --   | trace (show (start,ps,pe)) False = undefined
       pscan' (Just (s, sBumps)) ps pe
         | s `cornerEq` e = S.empty        -- illegal one-line view
         | ps > pe =                       -- reached end, scan next
             pscan n ptr l (d+1) (s, e) (sBumps, eBumps)
         | closed (l `at` tr (d, ps)) =    -- entering shadow, steep bump
             fromMaybe S.empty
               (do
                  let steepBump = dp2bump (d, ps)
                  ne <- lineOfHull Steep steepBump sBumps -- can fail?
                  let neBumps = addHull steepBump eBumps
                  return $
                    S.union
                      (pscan n ptr l (d+1) (s, ne) (sBumps, neBumps))
                      (pscan' Nothing (ps+1) pe))
         | otherwise =                     -- continue in light
             pscan' (Just (s, sBumps)) (ps+1) pe

       pscan' Nothing ps pe
         | ps > pe = S.empty               -- reached end while in shadow
         | open (l `at` tr (d, ps)) =      -- moving out of shadow, shallow bump
             fromMaybe S.empty
               (do
                  let shallowBump = dp2bump (d, ps)
                  ns <- lineOfHull Shallow shallowBump eBumps  -- can fail
                  let nsBumps = [shallowBump]
                  return $ pscan' (Just (ns, nsBumps)) (ps+1) pe)
{-TODO
         | closed (l `at` tr (d, ps+1)) && -- up-left is in shadow, too
           open (l `at` tr (d+1, ps+1)) =  -- the tile up between is open TODO:if closed, just add the tile
             fromMaybe S.empty             -- diagonal view through a corner
               (do
                  let shallowBump = dp2bump (d, ps)
                      steepBump   = dp2bump (d, ps+1)
                      nsBumps = [shallowBump]
                      neBumps = [steepBump]
                  ns <- lineOfHull Shallow shallowBump neBumps  -- can fail?
                  ne <- lineOfHull Steep steepBump nsBumps      -- can fail?
                  return $
                    S.union
                      (pscan n ptr l (d+1) (ns, ne) (nsBumps, neBumps))
                      (pscan' Nothing (ps+1) pe))-}
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

The Loc coordinates are cartesian, the Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and always looks at the first quadrant,
the (Distance, Progress) cordinates are mangled and not used for geometry.
-}
ptr0, ptr1, ptr2, ptr3 :: Loc -> Bump -> Loc
ptr0 (oy, ox) (B(y, x)) = (oy + y, ox + x)  -- first quadrant
ptr1 (oy, ox) (B(y, x)) = (oy - y, ox + x)  -- then clockwise
ptr2 (oy, ox) (B(y, x)) = (oy - y, ox - x)
ptr3 (oy, ox) (B(y, x)) = (oy + y, ox - x)

dp2bump :: (Distance, Progress) -> Bump
dp2bump (d, p) = B(p, d - p)

-- | The progress at which a line intersects the diagonal set of tiles
-- at the given distance.
intersectD :: WhichLine -> Line -> Distance -> Progress
intersectD which (B(y1, x1), B(y2, x2)) d =
  -- The diagonals of the tiles lie on the line y = 1 + d - x,
  -- hence the intersection point (y0, x0) has the following y0 coordinate:
  let (yq, yr) =
        ((1 + d)*(y2 - y1) + y1*x2 - x1*y2) `quotRem` ((x2 - x1) + (y2 - y1))
  in
   -- Corners do not block view, so if the line interects the very
   -- corner of 2 tiles, choose the tile that provides a larger view.
   if yr == 0 && which == Shallow then yq - 1 else yq
{-
y = 1 + d - x
y - y1 = (x - x1) (y2 - y1) / (x2 - x1)
y (x2 - x1) - y1 x2 = x (y2 - y1) - x1 y2
y (x2 - x1) - y1 x2 = (1 + d) (y2 - y1) - y (y2 - y1) - x1 y2
y (x2 - x1) + y (y2 - y1) = (1 + d) (y2 - y1) - x1 y2 + y1 x2
y = ((1 + d) (y2 - y1) + y1 x2 - x1 y2) / (x2 - x1 + y2 - y1)
-}

-- | Checks if the shallow and steep lines are equal,
-- but only if both come from the same corner on the diagonal of hero's tile.
-- If the two lines are equal, the view is a single line.
-- Since the hero's corners on the diagonal do not count for visibility,
-- a single line view originating from them is canceled,
-- after beging detected with this function. No coordinate is equal to 0.
cornerEq :: Line -> Line -> Bool
cornerEq (B(1, 0), B(ys, xs)) (B(1, 0), B(ye, xe)) = ys * xe == ye * xs
cornerEq (B(0, 1), B(ys, xs)) (B(0, 1), B(ye, xe)) = ys * xe == ye * xs
cornerEq _ _ = False

-- | Constructs steep or shallow line from the far point and the opposite
-- convex hull of bumps. Fails if the bumps force the line
-- out of the hero's tile. TODO: other special cases? remove these?
lineOfHullRaw :: WhichLine -> Bump -> ConvexHull -> Maybe Line
lineOfHullRaw which (B(0, 1)) [B(1, 0)] = Just (B(1, 1), B(1, 2))
lineOfHullRaw which (B(1, 0)) [B(0, 1)] = Just (B(1, 1), B(2, 1))
lineOfHullRaw which (B(y, x)) hull =
  case which of
    Shallow -> Just (B(1, 0), B(topLeft     (y, x)))  -- TODO
    Steep   -> Just (B(0, 1), B(bottomRight (y, x)))  -- TODO
    where
      topLeft     (y, x) = (y + 1, x)
      bottomRight (y, x) = (y, x + 1)

lineOfHull :: WhichLine -> Bump -> ConvexHull -> Maybe Line
lineOfHull which bump hull =
  case lineOfHullRaw which bump hull of
    Just (B(y1, x1), B(y2, x2))
      | y1 == y2 && x1 == x2 -> error "lineOfHull: invalid line"
    result -> result

-- | Adds a bump to a the convex hull of bumps represented as a list.
-- TODO: Can be optimized by removing some points from the list.
addHull :: Bump -> ConvexHull -> ConvexHull
addHull loc l = loc : l
