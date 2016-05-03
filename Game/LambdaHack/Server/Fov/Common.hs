-- | Common definitions for the Field of View algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for some more context and references.
module Game.LambdaHack.Server.Fov.Common
  ( -- * Current scan parameters
    Distance, Progress
    -- * Scanning coordinate system
  , Bump(..)
    -- * Geometry in system @Bump@
  , Line(..), ConvexHull, Edge, EdgeInterval
    -- * Assorted minor operations
  , maximal, steeper, addHull
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

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
