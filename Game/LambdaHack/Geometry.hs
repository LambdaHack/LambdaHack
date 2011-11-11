module Game.LambdaHack.Geometry
  ( Time, X, Y, shiftXY, movesXY, lenXY, euclidLenSq, normalLevelBound ) where

-- | Game time in turns. The time dimension.
type Time = Int

-- | Spacial dimensions, points and vectors.
type X = Int
type Y = Int

-- | Shift a point by a vector.
shiftXY :: (X, Y) -> (X, Y) -> (X, Y)
shiftXY (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- | Vectors of all unit moves, clockwise, starting north-west.
movesXY :: [(X, Y)]
movesXY = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- | The lenght of a vector in our metric where diagonal moves cost 1 move.
lenXY :: (X, Y) -> Int
lenXY (x, y) = max (abs x) (abs y)

-- | Squared euclidean length of a vector.
euclidLenSq :: (X, Y) -> Int
euclidLenSq (x, y) = let square a = a * a
                     in square x + square y

normalLevelBound :: (X, Y)
normalLevelBound = (79, 22)  -- TODO: query terminal size instead
