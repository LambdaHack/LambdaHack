-- | Basic cartesian geometry operations on 2D vectors.
module Game.LambdaHack.Common.VectorXY
  ( VectorXY(..), movesXY
  , chessDistXY, euclidDistSqXY, negXY, vicinityXY, vicinityCardinalXY
  ) where

import Game.LambdaHack.Common.PointXY

-- | 2D vectors in cartesian representation. Coordinates grow to the right
-- and down, so that the (0, 0) point is in the top-left corner of the screen.
data VectorXY = VectorXY
  { vx :: !X
  , vy :: !Y
  }
  deriving (Eq, Ord, Show)

-- | Shift a point by a vector.
shiftXY :: PointXY -> VectorXY -> PointXY
shiftXY (PointXY x0 y0) (VectorXY x1 y1) = PointXY (x0 + x1) (y0 + y1)

-- | Vectors of all unit moves in the chessboard metric,
-- clockwise, starting north-west.
movesXY :: [VectorXY]
movesXY =
  map (uncurry VectorXY)
    [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- | Vectors of all cardinal direction unit moves, clockwise, starting north.
movesCardinalXY :: [VectorXY]
movesCardinalXY = map (uncurry VectorXY) [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | The lenght of a vector in the chessboard metric,
-- where diagonal moves cost 1.
chessDistXY :: VectorXY -> Int
chessDistXY (VectorXY x y) = max (abs x) (abs y)

-- | Squared euclidean length of a vector.
euclidDistSqXY :: VectorXY -> Int
euclidDistSqXY (VectorXY x y) = x * x + y * y

-- | Reverse an arbirary vector.
negXY :: VectorXY -> VectorXY
negXY (VectorXY x y) = VectorXY (-x) (-y)

-- | All (8 at most) closest neighbours of a point within an area.
vicinityXY :: (X, Y, X, Y)  -- ^ limit the search to this area
           -> PointXY       -- ^ position to find neighbours of
           -> [PointXY]
vicinityXY area xy =
  [ res | dxy <- movesXY, let res = shiftXY xy dxy, insideXY res area ]

-- | All (4 at most) cardinal direction neighbours of a point within an area.
vicinityCardinalXY :: (X, Y, X, Y)  -- ^ limit the search to this area
                   -> PointXY       -- ^ position to find neighbours of
                   -> [PointXY]
vicinityCardinalXY area xy =
  [ res
  | dxy <- movesCardinalXY, let res = shiftXY xy dxy, insideXY res area ]
