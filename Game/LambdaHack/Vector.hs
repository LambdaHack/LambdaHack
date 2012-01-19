-- | Geografical directions implemented in an efficient way.
module Game.LambdaHack.Vector
  ( Dir, dirDistSq, diagonal, neg, moves, movesWidth, shift, towards
  ) where

import Data.Binary

import Game.LambdaHack.PointXY
import Game.LambdaHack.VectorXY
import Game.LambdaHack.Point
import Game.LambdaHack.Utils.Assert

-- | Vectors of length 1 (in our metric), that is, geographical directions.
-- Implemented as an offset in the linear framebuffer indexed by Loc.
-- A newtype to prevent mixing up with Loc itself.
-- The X size of the level has to be > 1 for the 'moves' list of
-- vectors to make sense.
newtype Dir = Dir Int deriving (Show, Eq)

instance Binary Dir where
  put (Dir dir) = put dir
  get = fmap Dir get

toDir :: X -> (X, Y) -> Dir
toDir lxsize (x, y) =
  assert (lxsize > 1 && chessDistXY (x, y) == 1 `blame` (lxsize, (x, y))) $
  Dir $ x + y * lxsize

fromDir :: X -> Dir -> (X, Y)
fromDir lxsize (Dir dir) =
  assert (chessDistXY res == 1 && fst res + snd res * lxsize == dir
          `blame` (lxsize, dir, res)) $
  res
 where
  (x, y) = (dir `mod` lxsize, dir `div` lxsize)
  -- Pick the vector's canonical form of length 1:
  res = if x > 1
        then (x - lxsize, y + 1)
        else (x, y)

-- | Squared euclidean distance between two directions.
dirDistSq :: X -> Dir -> Dir -> Int
dirDistSq lxsize dir0 dir1
  | (x0, y0) <- fromDir lxsize dir0, (x1, y1) <- fromDir lxsize dir1 =
  euclidDistSq ((y1 - y0), (x1 - x0))

-- | Checks whether a direction is diagonal, as opposed to cardinal.
diagonal :: X -> Dir -> Bool
diagonal lxsize dir | (x, y) <- fromDir lxsize dir =
  x * y /= 0

-- | Reverse a direction (vector).
neg :: Dir -> Dir
neg (Dir dir) = Dir (-dir)

-- | Directions of all unit moves, clockwise, starting north-west.
moves :: X -> [Dir]
moves lxsize = map (toDir lxsize) movesXY

-- | Directions of all unit moves, clockwise, starting north-west,
-- parameterized by level width.
movesWidth :: [X -> Dir]
movesWidth = map (flip toDir) movesXY

-- | Move one square in the given direction.
--
-- Particularly simple and fast implementation in the linear representation.
shift :: Loc -> Dir -> Loc
shift loc (Dir dir) = loc + dir

-- TODO: Perhaps produce all acceptable directions and let AI choose.
-- That would also eliminate the Doubles.
-- | Given two distinct locations, determine the direction in which one should
-- move from the first in order to get closer to the second.
-- Ignores obstacles. Of several equally good directions
-- (in the metric where diagonal moves cost 1) it picks the one that visually
-- (in the euclidean metric) would be the best.
towards :: X -> Loc -> Loc -> Dir
towards lxsize loc0 loc1
  | (x0, y0) <- fromLoc lxsize loc0, (x1, y1) <- fromLoc lxsize loc1 =
  assert (loc0 /= loc1 `blame` (loc0, loc1, x0, y0)) $
  let dx = x1 - x0
      dy = y1 - y0
      angle :: Double
      angle = atan (fromIntegral dy / fromIntegral dx) / (pi / 2)
      dxy | angle <= -0.75 = (0, -1)
          | angle <= -0.25 = (1, -1)
          | angle <= 0.25  = (1, 0)
          | angle <= 0.75  = (1, 1)
          | angle <= 1.25  = (0, 1)
          | otherwise =
              assert `failure` (lxsize, loc0, loc1, (x0, y0), (x1, y1))
  in if dx >= 0 then toDir lxsize dxy else neg (toDir lxsize dxy)
