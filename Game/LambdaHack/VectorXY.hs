-- | Basic cartesian geometry operations on 2D vectors.
module Game.LambdaHack.VectorXY
  ( VectorXY, shiftXY, movesXY, movesCardinalXY, chessDistXY, euclidDistSq
  ) where

import Game.LambdaHack.PointXY

-- | 2D vectors in cartesian representation.
type VectorXY = (X, Y)

-- | Shift a point by a vector.
shiftXY :: PointXY -> VectorXY -> PointXY
shiftXY (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- | Vectors of all unit moves in the chessboard metric,
-- clockwise, starting north-west.
movesXY :: [VectorXY]
movesXY = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- | Vectors of all cardinal direction unit moves, clockwise, starting north.
movesCardinalXY :: [VectorXY]
movesCardinalXY = [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | The lenght of a vector in the chessboard metric,
-- where diagonal moves cost 1.
chessDistXY :: VectorXY -> Int
chessDistXY (x, y) = max (abs x) (abs y)

-- | Squared euclidean length of a vector.
euclidDistSq :: VectorXY -> Int
euclidDistSq (x, y) = x * x + y * y
