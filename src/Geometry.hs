module Geometry
  ( Time, VDir(..), X, Y, shiftXY, movesXY ) where

-- | Game time in turns. (Placement in module Geometry is not ideal.)
type Time = Int

-- | Vertical directions.
data VDir = Up | Down
  deriving (Eq, Ord, Show, Enum, Bounded)

type X = Int
type Y = Int

shiftXY :: (X, Y) -> (X, Y) -> (X, Y)
shiftXY (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- | Vectors of all unit moves, clockwise, starting north-west.
movesXY :: [(X, Y)]
movesXY = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]
