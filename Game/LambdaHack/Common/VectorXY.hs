-- | Basic cartesian geometry operations on 2D vectors.
module Game.LambdaHack.Common.VectorXY
  ( VectorXY(..)
  ) where

import Game.LambdaHack.Common.PointXY

-- | 2D vectors in cartesian representation. Coordinates grow to the right
-- and down, so that the (0, 0) point is in the top-left corner of the screen.
data VectorXY = VectorXY
  { vx :: !X
  , vy :: !Y
  }
  deriving (Eq, Ord, Show)
