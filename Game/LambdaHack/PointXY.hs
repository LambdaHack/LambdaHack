-- | Basic cartesian geometry operations on 2D points.
module Game.LambdaHack.PointXY
  ( X, Y, PointXY(..), fromTo, sortPointXY, blaXY
  ) where

import qualified Data.List as L

import Game.LambdaHack.Utils.Assert

-- | Spacial dimension for points and vectors.
type X = Int

-- | Spacial dimension for points and vectors.
type Y = Int

-- | 2D points in cartesian representation.
newtype PointXY = PointXY (X, Y)
  deriving (Eq, Ord)

instance Show PointXY where
  show (PointXY (x, y)) = show (x, y)

-- | A list of all points on a straight vertical or straight horizontal line
-- between two points. Fails if no such line exists.
fromTo :: PointXY -> PointXY -> [PointXY]
fromTo (PointXY (x0, y0)) (PointXY (x1, y1)) =
 let result
       | x0 == x1 = L.map (\ y -> PointXY (x0, y)) (fromTo1 y0 y1)
       | y0 == y1 = L.map (\ x -> PointXY (x, y0)) (fromTo1 x0 x1)
       | otherwise = assert `failure` ((x0, y0), (x1, y1))
 in result

fromTo1 :: Int -> Int -> [Int]
fromTo1 x0 x1
  | x0 <= x1  = [x0..x1]
  | otherwise = [x0,x0-1..x1]

-- | Sort the sequence of two points, in the derived lexicographic order.
sortPointXY :: (PointXY, PointXY) -> (PointXY, PointXY)
sortPointXY (a, b) | a <= b    = (a, b)
                   | otherwise = (b, a)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA).
-- Includes the first point and goes through the second to infinity.
blaXY :: Int -> PointXY -> PointXY -> [PointXY]
blaXY eps (PointXY (x0, y0)) (PointXY (x1, y1)) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      bw = balancedWord p q (eps `mod` (max 1 q))
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in L.map PointXY $ walk bw (x0, y0)
