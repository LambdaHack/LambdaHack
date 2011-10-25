module FOV.Common
  ( Interval, Distance, Progress
  , Bump(..)
  , Line, ConvexHull, Edge, EdgeInterval
  , divUp
  , maximal
  , steeper
  , addHull
  ) where

import qualified Data.List as L

import Geometry

type Interval = (Rational, Rational)
type Distance = Int
type Progress = Int

-- | Coordinates of points in a single quadrant.
-- (The first quadrant for Permissive FOV, hence both coordinates positive,
-- a adjacent diagonal halves of the first and the second quadrant
-- for Digital FOV, hence y positive.)
newtype Bump = B Loc
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
