{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Basic cartesian geometry operations on 2D vectors.
module Game.LambdaHack.Common.VectorXY
  ( VectorXY(..), shiftXY, movesXY, movesCardinalXY
  , chessDistXY, euclidDistSqXY, negXY
  ) where

import Data.Binary

import Game.LambdaHack.Common.PointXY

-- | 2D vectors in cartesian representation.
newtype VectorXY = VectorXY (X, Y)
  deriving (Eq, Ord, Show, Read, Binary)

-- | Shift a point by a vector.
shiftXY :: PointXY -> VectorXY -> PointXY
shiftXY (PointXY (x0, y0)) (VectorXY (x1, y1)) = PointXY (x0 + x1, y0 + y1)

-- | Vectors of all unit moves in the chessboard metric,
-- clockwise, starting north-west.
movesXY :: [VectorXY]
movesXY =
  map VectorXY
    [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- | Vectors of all cardinal direction unit moves, clockwise, starting north.
movesCardinalXY :: [VectorXY]
movesCardinalXY = map VectorXY [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | The lenght of a vector in the chessboard metric,
-- where diagonal moves cost 1.
chessDistXY :: VectorXY -> Int
chessDistXY (VectorXY (x, y)) = max (abs x) (abs y)

-- | Squared euclidean length of a vector.
euclidDistSqXY :: VectorXY -> Int
euclidDistSqXY (VectorXY (x, y)) = x * x + y * y

-- | Reverse an arbirary vector.
negXY :: VectorXY -> VectorXY
negXY (VectorXY (x, y)) = VectorXY (-x, -y)
