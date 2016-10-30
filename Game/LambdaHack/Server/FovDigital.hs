-- | DFOV (Digital Field of View) implemented according to specification at <http://roguebasin.roguelikedevelopment.org/index.php?title=Digital_field_of_view_implementation>.
-- This fast version of the algorithm, based on "PFOV", has AFAIK
-- never been described nor implemented before.
module Game.LambdaHack.Server.FovDigital
  ( scan
    -- * Scanning coordinate system
  , Bump(..)
    -- * Assorted minor operations
#ifdef EXPOSE_INTERNAL
    -- * Current scan parameters
  , Distance, Progress
    -- * Geometry in system @Bump@
  , Line(..), ConvexHull, Edge, EdgeInterval
    -- * Internal operations
  , maximal, steeper, addHull
  , dline, dsteeper, intersect, _debugSteeper, _debugLine
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude hiding (intersect)

-- | Distance from the (0, 0) point where FOV originates.
type Distance = Int
-- | Progress along an arc with a constant distance from (0, 0).
type Progress = Int

-- | Rotated and translated coordinates of 2D points, so that the points fit
-- in a single quadrant area (e, g., quadrant I for Permissive FOV, hence both
-- coordinates positive; adjacent diagonal halves of quadrant I and II
-- for Digital FOV, hence y positive).
-- The special coordinates are written using the standard mathematical
-- coordinate setup, where quadrant I, with x and y positive,
-- is on the upper right.
data Bump = B
  { bx :: !Int
  , by :: !Int
  }
  deriving Show

-- | Straight line between points.
data Line = Line !Bump !Bump
  deriving Show

-- | Convex hull represented as a list of points.
type ConvexHull   = [Bump]
-- | An edge (comprising of a line and a convex hull)
-- of the area to be scanned.
type Edge         = (Line, ConvexHull)
-- | The area left to be scanned, delimited by edges.
type EdgeInterval = (Edge, Edge)

-- | Calculates the list of tiles, in @Bump@ coordinates, visible from (0, 0),
-- within the given sight range.
scan :: Distance        -- ^ visiblity distance
     -> (Bump -> Bool)  -- ^ clear tile predicate
     -> [Bump]
{-# INLINABLE scan #-}
scan r isClear = assert (r > 0 `blame` r) $
  -- The scanned area is a square, which is a sphere in the chessboard metric.
  dscan [] 1 ( (Line (B 1 0) (B (-r) r), [B 0 0])
             , (Line (B 0 0) (B (r+1) r), [B 1 0]) )
 where
  dscan :: [Bump] -> Distance -> EdgeInterval -> [Bump]
  dscan !accDscan !d ( s0@(!sl{-shallow line-}, !sHull0)
                     , e@(!el{-steep line-}, !eHull) ) =

    let !ps0 = let (n, k) = intersect sl d  -- minimal progress to consider
               in n `div` k
        !pe = let (n, k) = intersect el d   -- maximal progress to consider
                -- Corners obstruct view, so the steep line, constructed
                -- from corners, is itself not a part of the view,
                -- so if its intersection with the line of diagonals is only
                -- at a corner, choose the diamond leading to a smaller view.
              in -1 + n `divUp` k
        inside = [B p d | p <- [ps0..pe]] ++ accDscan
        outside
          | d >= r = inside
          | isClear (B ps0 d) = mscanVisible inside s0 (ps0+1)  -- start visible
          | otherwise = mscanShadowed inside (ps0+1)          -- start in shadow

        {-# INLINE findInInterval #-}
        findInInterval f xstart xend =
          let g !x | x > xend = Nothing
                   | f x = Just x
                   | otherwise = g (x + 1)
          in g xstart

        {-# INLINE bump #-}
        bump px = B px d

        -- We're in a visible interval.
        mscanVisible :: [Bump] -> Edge -> Progress -> [Bump]
--        {-# INLINE mscanVisible #-}
        mscanVisible !acc !s !ps =
          case findInInterval (not . isClear . bump) ps pe of
            Just px ->  -- entering shadow
              let {-# INLINE steepBump #-}
                  steepBump = bump px
                  gte :: Bump -> Bump -> Bool
                  {-# INLINE gte #-}
                  gte = dsteeper steepBump
                  nep = maximal gte (snd s)
                  neHull = addHull gte steepBump eHull
                  accNew = dscan acc (d+1) (s, (dline nep steepBump, neHull))
              in mscanShadowed accNew (px+1)
--                  accNew = mscanShadowed acc (px+1)
--              in dscan accNew (d+1) (s, (dline nep steepBump, neHull))
            Nothing -> dscan acc (d+1) (s, e)  -- reached end, scan next

        -- We're in a shadowed interval.
        mscanShadowed :: [Bump] -> Progress -> [Bump]
        mscanShadowed !acc !ps =
          case findInInterval (isClear . bump) ps pe of
            Just px ->  -- moving out of shadow
              let {-# INLINE shallowBump #-}
                  shallowBump = B px d
                  gte :: Bump -> Bump -> Bool
                  {-# INLINE gte #-}
                  gte = flip $ dsteeper shallowBump
                  nsp = maximal gte eHull
                  nsHull = addHull gte shallowBump sHull0
              in mscanVisible acc (dline nsp shallowBump, nsHull) (px+1)
            Nothing -> acc  -- reached end while in shadow

    in assert (r >= d && d >= 0 && pe >= ps0 `blame` (r,d,s0,e,ps0,pe)) $
       outside

-- | Maximal element of a non-empty list. Prefers elements from the rear,
-- which is essential for PFOV, to avoid ill-defined lines.
maximal :: (a -> a -> Bool) -> [a] -> a
{-# INLINE maximal #-}
maximal gte = foldl1' (\acc e -> if gte e acc then e else acc)

-- | Check if the line from the second point to the first is more steep
-- than the line from the third point to the first. This is related
-- to the formal notion of gradient (or angle), but hacked wrt signs
-- to work fast in this particular setup. Returns True for ill-defined lines.
steeper :: Bump -> Bump -> Bump -> Bool
{-# INLINE steeper #-}
steeper (B xf yf) (B x1 y1) (B x2 y2) =
  (yf - y1)*(xf - x2) >= (yf - y2)*(xf - x1)

-- | Extends a convex hull of bumps with a new bump. Nothing needs to be done
-- if the new bump already lies within the hull. The first argument is
-- typically `steeper`, optionally negated, applied to the second argument.
addHull :: (Bump -> Bump -> Bool)  -- ^ a comparison function
        -> Bump                    -- ^ a new bump to consider
        -> ConvexHull  -- ^ a convex hull of bumps represented as a list
        -> ConvexHull
{-# INLINE addHull #-}
addHull gte new = (new :) . go
 where
  go (a:b:cs) | gte a b = go (b:cs)
  go l = l

-- | Create a line from two points. Debug: check if well-defined.
dline :: Bump -> Bump -> Line
{-# INLINE dline #-}
dline p1 p2 =
  let line = Line p1 p2
  in
#ifdef WITH_EXPENSIVE_ASSERTIONS
    assert (uncurry blame $ _debugLine line)
#endif
      line

-- | Compare steepness of @(p1, f)@ and @(p2, f)@.
-- Debug: Verify that the results of 2 independent checks are equal.
dsteeper :: Bump -> Bump -> Bump -> Bool
{-# INLINE dsteeper #-}
dsteeper f p1 p2 =
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (res == _debugSteeper f p1 p2)
#endif
    res
 where res = steeper f p1 p2

-- | The X coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of diamonds at distance
-- @d@ from (0, 0).
intersect :: Line -> Distance -> (Int, Int)
{-# INLINE intersect #-}
intersect (Line (B x y) (B xf yf)) d =
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (allB (>= 0) [y, yf])
#endif
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

The Point coordinates are cartesian. The Bump coordinates are cartesian,
translated so that the hero is at (0, 0) and rotated so that he always
looks at the first (rotated 45 degrees) quadrant. The (Progress, Distance)
cordinates coincide with the Bump coordinates, unlike in PFOV.
-}

-- | Debug functions for DFOV:

-- | Debug: calculate steeper for DFOV in another way and compare results.
_debugSteeper :: Bump -> Bump -> Bump -> Bool
{-# INLINE _debugSteeper #-}
_debugSteeper f@(B _xf yf) p1@(B _x1 y1) p2@(B _x2 y2) =
  assert (allB (>= 0) [yf, y1, y2]) $
  let (n1, k1) = intersect (Line p1 f) 0
      (n2, k2) = intersect (Line p2 f) 0
  in n1 * k2 >= k1 * n2

-- | Debug: check if a view border line for DFOV is legal.
_debugLine :: Line -> (Bool, String)
{-# INLINE _debugLine #-}
_debugLine line@(Line (B x1 y1) (B x2 y2))
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
  (n, k)  = line `intersect` 0
  (q, r)  = if k == 0 then (0, 0) else n `divMod` k
  crossL0 = q < 0  -- q truncated toward negative infinity
  crossG1 = q >= 1 && (q > 1 || r /= 0)
