-- | Basic cartesian geometry operations.
module Game.LambdaHack.Geometry
  ( -- * Geometry
    X, Y, shiftXY, movesXY, movesCardinalXY, lenXY, euclidDistSq
    -- * Assorted
  , normalLevelBound, Time, divUp
  ) where

-- TODO: rename to PointXY, create VectorXy, rename Loc to Point,
-- Dir to UnitVector, create type synonims with these names.

-- | Spacial dimensions for points and vectors.
type X = Int
type Y = Int

-- | Shift a point by a vector.
shiftXY :: (X, Y) -> (X, Y) -> (X, Y)
shiftXY (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- | Vectors of all unit moves, clockwise, starting north-west.
movesXY :: [(X, Y)]
movesXY = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- | Vectors of all cardinal unit moves, clockwise, starting north.
movesCardinalXY :: [(X, Y)]
movesCardinalXY = [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | The lenght of a vector in our metric where diagonal moves cost 1 move.
lenXY :: (X, Y) -> Int
lenXY (x, y) = max (abs x) (abs y)

-- | Squared euclidean length of a vector.
euclidDistSq :: (X, Y) -> Int
euclidDistSq (x, y) = let square a = a * a
                     in square x + square y

-- TODO: move all below somewhere else later on

-- | Level bounds. TODO: query terminal size instead and scroll view.
normalLevelBound :: (X, Y)
normalLevelBound = (79, 21)

-- | Game time in turns. The time dimension.
type Time = Int

-- | Integer division, rounding up.
divUp :: Int -> Int -> Int
divUp n k = (n + k - 1) `div` k
