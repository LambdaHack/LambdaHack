-- | Basic cartesian geometry operations on 2D points.
module Game.LambdaHack.Common.PointXY
  ( X, Y, PointXY(..)
  ) where

-- | Spacial dimension for points and vectors.
type X = Int

-- | Spacial dimension for points and vectors.
type Y = Int

-- | 2D points in cartesian representation. Coordinates grow to the right
-- and down, so that the (0, 0) point is in the top-left corner of the screen.
-- Coordinates are never negative.
--
-- Note: If we used this instead of @Point@, we'd need, e.g.,
-- an extra addition per FOV point and per AI speculative move,
-- plus some possible extra memory accesses to the individual coordinates.
-- Even more serious is an extra addition and access per EnumMap lookup,
-- though in the computation-intensive cases of FOV and AI, the extra
-- operations were already there, performed before lookup.
data PointXY = PointXY
  { px :: !X
  , py :: !Y
  }
  deriving (Eq, Ord)

instance Show PointXY where
  show (PointXY x y) = show (x, y)
