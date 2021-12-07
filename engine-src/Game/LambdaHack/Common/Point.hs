{-# LANGUAGE DeriveGeneric #-}
-- | Basic operations on 2D points represented as linear offsets.
module Game.LambdaHack.Common.Point
  ( Point(..), PointI
  , chessDist, euclidDistSq, adjacent, bla, fromTo
  , originPoint, insideP
  , speedupHackXSize
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , blaXY, balancedWord
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import           Data.Int (Int32)
import qualified Data.Primitive.PrimArray as PA
import           GHC.Generics (Generic)
import           Test.QuickCheck

import Game.LambdaHack.Definition.Defs

-- | This is a hack to pass the X size of the dungeon, defined
-- in game content, to the @Enum@ instances of @Point@ and @Vector@.
-- This is already slower and has higher allocation than
-- hardcoding the value, so passing the value explicitly to
-- a generalization of the @Enum@ conversions is out of the question.
-- Perhaps this can be done cleanly and efficiently at link-time
-- via Backpack, but it's probably not supported yet by GHCJS (not verified).
-- For now, we need to be careful never to modify this array,
-- except for setting it at program start before it's used for the first time.
-- Which is easy, because @Point@ is never mentioned in content definitions.
-- The @PrimArray@ has much smaller overhead than @IORef@
-- and reading from it looks cleaner, hence its use.
speedupHackXSize :: PA.PrimArray X
{-# NOINLINE speedupHackXSize #-}
speedupHackXSize = PA.primArrayFromList [80]  -- updated at program startup

-- | 2D points in cartesian representation. Coordinates grow to the right
-- and down, so that the (0, 0) point is in the top-left corner
-- of the screen. Coordinates are never negative
-- (unlike for 'Game.LambdaHack.Common.Vector.Vector')
-- and the @X@ coordinate never reaches the screen width as read
-- from 'speedupHackXSize'.
data Point = Point
  { px :: X
  , py :: Y
  }
  deriving (Eq, Ord, Generic)

instance Show Point where
  show (Point x y) = show (x, y)

instance Binary Point where
  put = put . (toIntegralCrash :: Int -> Int32) . fromEnum
  get = fmap (toEnum . (fromIntegralWrap :: Int32 -> Int)) get
    -- `fromIntegralWrap` is fine here, because we converted the integer
    -- in the opposite direction first, so it fits even in 31 bit `Int`

-- Note that @Ord@ on @Int@ is not monotonic wrt @Ord@ on @Point@.
-- We need to keep it that way, because we want close xs to have close indexes,
-- e.g., adjacent points in line to have adjacent enumerations,
-- because some of the screen layout and most of processing is line-by-line.
-- Consequently, one can use EM.fromDistinctAscList
-- on @(1, 8)..(10, 8)@, but not on @(1, 7)..(10, 9)@.
instance Enum Point where
  fromEnum Point{..} =
    let !xsize = PA.indexPrimArray speedupHackXSize 0
    in
#ifdef WITH_EXPENSIVE_ASSERTIONS
       assert (px >= 0 && py >= 0 && px < xsize
              `blame` "invalid point coordinates"
              `swith` (px, py))
#endif
         (px + py * xsize)
  toEnum n = let !xsize = PA.indexPrimArray speedupHackXSize 0
                 (py, px) = n `quotRem` xsize
             in Point{..}

instance Arbitrary Point where
  arbitrary = do
    let xsize = PA.indexPrimArray speedupHackXSize 0
    n <- getSize
    Point <$> choose (0, min n (xsize - 1))
          <*> choose (0, n)

-- | Enumeration representation of @Point@.
type PointI = Int

-- This is hidden from Haddock, but run by doctest:
-- $
-- prop> (toEnum :: PointI -> Point) (fromEnum p) == p
-- prop> \ (NonNegative i) -> (fromEnum :: Point -> PointI) (toEnum i) == i

-- | The distance between two points in the chessboard metric.
--
-- >>> chessDist (Point 0 0) (Point 0 0)
-- 0
-- >>> chessDist (Point (-1) 0) (Point 0 0)
-- 1
-- >>> chessDist (Point (-1) 0) (Point (-1) 1)
-- 1
-- >>> chessDist (Point (-1) 0) (Point 0 1)
-- 1
-- >>> chessDist (Point (-1) 0) (Point 1 1)
-- 2
--
-- prop> chessDist p1 p2 >= 0
-- prop> chessDist p1 p2 ^ (2 :: Int) <= euclidDistSq p1 p2
chessDist :: Point -> Point -> Int
chessDist (Point x0 y0) (Point x1 y1) = max (abs (x1 - x0)) (abs (y1 - y0))

-- | Squared euclidean distance between two points.
euclidDistSq :: Point -> Point -> Int
euclidDistSq (Point x0 y0) (Point x1 y1) =
  (x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int)

-- | Checks whether two points are adjacent on the map
-- (horizontally, vertically or diagonally).
adjacent :: Point -> Point -> Bool
{-# INLINE adjacent #-}
adjacent s t = chessDist s t == 1

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA).
-- Skips the source point and goes through the second point to infinity.
-- Gives @Nothing@ if the points are equal. The target is given as @Point@,
-- not @PointI@, to permit aiming out of the level, e.g., to get
-- uniform distributions of directions for explosions close to the edge
-- of the level.
-- |
-- >>> bla 0 (Point 0 0) (Point 0 0) 
-- Nothing
-- >>> take 3 $ fromJust $ bla 0 (Point 0 0) (Point 1 0) 
-- [(1,0),(2,0),(3,0)]
-- >>> take 3 $ fromJust $ bla 0 (Point 0 0) (Point 0 1) 
-- [(0,1),(0,2),(0,3)]
-- >>> take 3 $ fromJust $ bla 0 (Point 0 0) (Point 1 1)
-- [(1,1),(2,2),(3,3)]
bla :: Int -> Point -> Point -> Maybe [Point]
bla eps source target =
  if source == target then Nothing
  else Just $ tail $ blaXY eps source target

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA). Includes the source point
-- and goes through the target point to infinity.
-- | >>> take 4 $ blaXY 0 (Point 0 0) (Point 2 0)
-- [(0,0),(1,0),(2,0),(3,0)]
blaXY :: Int -> Point -> Point -> [Point]
blaXY eps (Point x0 y0) (Point x1 y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      bw = balancedWord p q (eps `mod` max 1 q)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in map (uncurry Point) $ walk bw (x0, y0)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/index.php?title=Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

-- | A list of all points on a straight vertical or straight horizontal line
-- between two points. Fails if no such line exists.
-- >>> fromTo (Point 0 0) (Point 2 0)
-- [(0,0),(1,0),(2,0)]
fromTo :: Point -> Point -> [Point]
fromTo (Point x0 y0) (Point x1 y1) =
 let fromTo1 :: Int -> Int -> [Int]
     fromTo1 z0 z1
       | z0 <= z1  = [z0..z1]
       | otherwise = [z0,z0-1..z1]
     result
       | x0 == x1 = map (Point x0) (fromTo1 y0 y1)
       | y0 == y1 = map (`Point` y0) (fromTo1 x0 x1)
       | otherwise = error $ "diagonal fromTo"
                             `showFailure` ((x0, y0), (x1, y1))
 in result

originPoint :: Point
originPoint = Point 0 0

-- | Checks that a point belongs to an area.
insideP :: (X, Y, X, Y) -> Point -> Bool
{-# INLINE insideP #-}
insideP (x0, y0, x1, y1) (Point x y) = x1 >= x && x >= x0 && y1 >= y && y >= y0
