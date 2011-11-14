module Game.LambdaHack.FOV.Common
  ( Distance, Progress
  , Bump(..)
  , Line, ConvexHull, Edge, EdgeInterval
  , divUp, maximal, steeper, addHull
  ) where

import qualified Data.List as L

import Game.LambdaHack.Geometry

type Distance = Int
type Progress = Int

-- | Rotated and translated coordinates of 2D points, so that the points fit
-- in a single quadrant area (quadrant I for Permissive FOV, hence both
-- coordinates positive, and adjacent diagonal halves of quadrant I and II
-- for Digital FOV, hence y positive).
-- The coordinates are written using the standard mathematical coordinate setup,
-- where quadrant I, with x and y positive, is on the upper right.
newtype Bump = B (X, Y)
  deriving (Show)

type Line         = (Bump, Bump)
type ConvexHull   = [Bump]
type Edge         = (Line, ConvexHull)
type EdgeInterval = (Edge, Edge)

-- | Integer division, rounding up.
divUp :: Int -> Int -> Int
divUp n k = (n + k - 1) `div` k

-- | Maximal element of a non-empty list. Prefers elements from the rear,
-- which is essential for PFOV, to avoid ill-defined lines.
maximal :: (a -> a -> Bool) -> [a] -> a
maximal gte = L.foldl1' (\ acc e -> if gte e acc then e else acc)

-- | Check if the line from the second point to the first is more steep
-- than the line from the third point to the first. This is related
-- to the formal notion of gradient (or angle), but hacked wrt signs
-- to work in this particular setup. Returns True for ill-defined lines.
steeper :: Bump ->  Bump -> Bump -> Bool
steeper (B(xf, yf)) (B(x1, y1)) (B(x2, y2)) =
  (yf - y1)*(xf - x2) >= (yf - y2)*(xf - x1)

-- | Adds a bump to the convex hull of bumps represented as a list.
addHull :: (Bump -> Bump -> Bool) -> Bump -> ConvexHull -> ConvexHull
addHull gte d l =
  case l of
    a:b:cs ->
      if gte a b
      then addHull gte d (b:cs)
      else d : l
    _ -> d : l
