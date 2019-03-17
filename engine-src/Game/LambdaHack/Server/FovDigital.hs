{-# OPTIONS_GHC -ddump-simpl -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -ddump-to-file -dsuppress-uniques #-}
-- | DFOV (Digital Field of View) implemented according to specification at <http://roguebasin.roguelikedevelopment.org/index.php?title=Digital_field_of_view_implementation>.
-- This fast version of the algorithm, based on "PFOV", has AFAIK
-- never been described nor implemented before.
--
-- The map is processed in depth-first-search manner, that is, as soon
-- as we detect on obstacle we move away from the viewer up to the
-- FOV radius and then restart on the other side of the obstacle.
-- This has better cache behaviour than breadth-firsts-search,
-- where we would process all tiles equally distant from the viewer
-- in the same round, because then we'd need to keep the many convex hulls
-- and edges, not just a single set, and we'd potentially traverse all
-- of them each round.
module Game.LambdaHack.Server.FovDigital
  ( scan
    -- * Scanning coordinate system
  , Bump(..)
    -- * Assorted minor operations
#ifdef EXPOSE_INTERNAL
    -- * Current scan parameters
  , Distance, Progress
    -- * Geometry in system @Bump@
  , Line(..), ConvexHull(..), CHull(..), Edge, EdgeInterval
    -- * Internal operations
  , steepestInHull, foldlCHull', addToHull, addToHullGo
  , createLine, steepness, intersect
  , _debugSteeper, _debugLine
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude hiding (intersect)

import Game.LambdaHack.Core.Point (PointI)

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
  { bx :: Int
  , by :: Int
  }
  deriving Show

-- | Two strict orderings of lines with a common point.
data LineOrdering = Steeper | Shallower

-- | Straight line between points.
data Line = Line Bump Bump
  deriving Show

-- | Convex hull represented as a non-empty list of points.
data ConvexHull = ConvexHull Bump CHull
  deriving Show

data CHull =
    CHNil
  | CHCons Bump CHull
  deriving Show

-- | An edge (comprising of a line and a convex hull)
-- of the area to be scanned.
type Edge = (Line, ConvexHull)

-- | The area left to be scanned, delimited by edges.
type EdgeInterval = (Edge, Edge)

-- | Calculates the list of tiles, in @Bump@ coordinates, visible from (0, 0),
-- within the given sight range.
scan :: Distance          -- ^ visiblity distance
     -> (PointI -> Bool)  -- ^ position visually clear predicate
     -> (Bump -> PointI)  -- ^ coordinate transformation
     -> [PointI]
{-# INLINE scan #-}
scan !r isClear tr =
#ifdef WITH_EXPENSIVE_ASSERTIONS
 assert (r > 0 `blame` r) $  -- not really expensive, but obfuscates Core
#endif
  -- The scanned area is a square, which is a sphere in the chessboard metric.
  dscan 1 ( (Line (B 1 0) (B (-r) r), ConvexHull (B 0 0) CHNil)
          , (Line (B 0 0) (B (r+1) r), ConvexHull (B 1 0) CHNil) )
 where
  dscan :: Distance -> EdgeInterval -> [PointI]
  {-# INLINE dscan #-}
  dscan !d ( (sl{-shallow line-}, sHull), (el{-steep line-}, eHull) ) =
    dgo d sl sHull el eHull

  -- Speed (mosty JS), but generally convincing GHC to unbox stuff.
  dgo :: Distance -> Line -> ConvexHull -> Line -> ConvexHull -> [PointI]
  dgo !d !sl sHull !el eHull =  -- @sHull@ and @eHull@ may be unused

    let !ps0 = let (n, k) = intersect sl d  -- minimal progress to consider
               in n `div` k
        !pe = let (n, k) = intersect el d   -- maximal progress to consider
                -- Corners obstruct view, so the steep line, constructed
                -- from corners, is itself not a part of the view,
                -- so if its intersection with the line of diagonals is only
                -- at a corner, choose the diamond leading to a smaller view.
              in -1 + n `divUp` k
        outside =
          if d < r
          then let !trBump = bump ps0
               in if isClear trBump
                  then trBump : mscanVisible sl sHull (ps0+1)  -- start visible
                  else trBump : mscanShadowed (ps0+1)    -- start in shadow
          else map bump [ps0..pe]

        bump :: Progress -> PointI
        bump !px = tr $ B px d

        -- We're in a visible interval.
        mscanVisible :: Line -> ConvexHull -> Progress -> [PointI]
        {-# INLINE mscanVisible #-}
        mscanVisible line hull ps1 = goVisible ps1
         where
          goVisible :: Progress -> [PointI]
          goVisible !ps =
            if ps <= pe
            then let !trBump = bump ps
                 in if isClear trBump  -- not entering shadow
                    then trBump : goVisible (ps+1)
                    else let steepBump = B ps d
                             nep = steepestInHull Shallower steepBump  hull
                             neLine = createLine nep steepBump
                             neHull = addToHull Shallower steepBump eHull
                         in trBump : dgo (d+1) line hull neLine neHull
                            ++ mscanShadowed (ps+1)
                              -- note how we recursively scan more and more
                              -- distant tiles, up to the FOV radius,
                              -- before starting to process the shadow
            else dgo (d+1) line hull el eHull  -- reached end, scan next

        -- We're in a shadowed interval.
        mscanShadowed :: Progress -> [PointI]
        mscanShadowed !ps =
          if ps <= pe
          then let !trBump = bump ps
               in if not $ isClear trBump  -- not moving out of shadow
                  then trBump : mscanShadowed (ps+1)
                  else let shallowBump = B ps d
                           nsp = steepestInHull Steeper shallowBump eHull
                           nsLine = createLine nsp shallowBump
                           nsHull = addToHull Steeper shallowBump sHull
                       in trBump : mscanVisible nsLine nsHull (ps+1)
          else []  -- reached end while in shadow

    in
#ifdef WITH_EXPENSIVE_ASSERTIONS
      assert (r >= d && d >= 0 && pe >= ps0
              `blame` (r,d,sl,sHull,el,eHull,ps0,pe))
#endif
        outside

-- | Specialized implementation for speed in the inner loop. Not partial.
steepestInHull :: LineOrdering -> Bump -> ConvexHull -> Bump
{-# NOINLINE steepestInHull #-}
steepestInHull !lineOrdering !new (ConvexHull !b !ch) = foldlCHull' max' b ch
 where max' !x !y = if steepness lineOrdering new x y then x else y

-- | Standard @foldl'@ over @CHull@.
foldlCHull' :: (a -> Bump -> a) -> a -> CHull -> a
{-# INLINE foldlCHull' #-}
foldlCHull' f z0 ch0 = fgo z0 ch0
 where fgo !z CHNil = z
       fgo z (CHCons b ch) = fgo (f z b) ch

-- | Extends a convex hull of bumps with a new bump. The new bump makes
-- some old bumps unnecessary, e.g. those that are joined with the new steep
-- bump with lines that are not shallower than any newer lines in the hull.
-- Removing such unnecessary bumps slightly speeds up computation
-- of 'steepestInHull'.
--
-- The recursive @go@ seems spurious, but it's called each time with
-- potentially different comparison predicate, so it's necessary.
addToHull :: LineOrdering  -- ^ the line ordering to use
          -> Bump          -- ^ a new bump to consider
          -> ConvexHull    -- ^ a convex hull of bumps represented as a list
          -> ConvexHull
{-# INLINE addToHull #-}
addToHull lineOrdering new (ConvexHull old ch) =
  ConvexHull new $ addToHullGo lineOrdering new $ CHCons old ch

-- This worker is needed to avoid returing a pair (new, result)
-- and also packing new (steepBump/shallowBump) twice, losing sharing.
addToHullGo :: LineOrdering -> Bump -> CHull -> CHull
{-# NOINLINE addToHullGo #-}
addToHullGo !lineOrdering !new = hgo
 where
  hgo :: CHull -> CHull
  hgo (CHCons a ch@(CHCons b _)) | not (steepness lineOrdering new b a) = hgo ch
  hgo ch = ch

-- | Create a line from two points.
--
-- Debug: check if well-defined.
createLine :: Bump -> Bump -> Line
{-# INLINE createLine #-}
createLine p1 p2 =
  let line = Line p1 p2
  in
#ifdef WITH_EXPENSIVE_ASSERTIONS
    assert (uncurry blame $ _debugLine line)
#endif
      line

-- | Strictly compare steepness of @(p1, f)@ and @(p2, f)@,
-- according to the @LineOrdering@ given. This is related
-- to the formal notion of gradient (or angle), but hacked wrt signs
-- to work fast in this particular setup.
--
-- Debug: Verify that the results of 2 independent checks are equal.
steepness :: LineOrdering -> Bump -> Bump -> Bump -> Bool
{-# INLINE steepness #-}
steepness lineOrdering (B xf yf) (B x1 y1) (B x2 y2) =
  let gradient1 = (yf - y2) * (xf - x1)
      gradient2 = (yf - y1) * (xf - x2)
      res = case lineOrdering of
        Steeper -> gradient1 > gradient2
        Shallower -> gradient1 < gradient2
  in
#ifdef WITH_EXPENSIVE_ASSERTIONS
     let sign = case lineOrdering of
           Steeper -> GT
           Shallower -> LT
     in assert (res == (sign == _debugSteeper (B xf yf) (B x1 y1) (B x2 y2)))
#endif
     res

-- | The X coordinate, represented as a fraction, of the intersection of
-- a given line and the line of diagonals of diamonds at distance
-- @d@ from (0, 0).
--
-- Debug: check that the line fits in the upper half-plane.
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

-- | Debug: calculate steepness for DFOV in another way and compare results.
_debugSteeper :: Bump -> Bump -> Bump -> Ordering
{-# INLINE _debugSteeper #-}
_debugSteeper f@(B _xf yf) p1@(B _x1 y1) p2@(B _x2 y2) =
  assert (allB (>= 0) [yf, y1, y2]) $
  let (n1, k1) = intersect (Line p1 f) 0
      (n2, k2) = intersect (Line p2 f) 0
  in compare (k1 * n2) (n1 * k2)

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
