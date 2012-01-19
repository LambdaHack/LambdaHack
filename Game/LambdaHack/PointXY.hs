-- | Basic cartesian geometry operations on 2D points.
module Game.LambdaHack.PointXY
  ( -- * Geometry
    X, Y, PointXY
    -- * Assorted
  , normalLevelBound, Time, divUp
  ) where

-- | Spacial dimensions for points and vectors.
type X = Int
type Y = Int

-- | 2D points in cartesian representation.
type PointXY = (X, Y)

-- TODO: move all below somewhere else later on

-- | Level bounds. TODO: query terminal size instead and scroll view.
normalLevelBound :: (X, Y)
normalLevelBound = (79, 21)

-- | Game time in turns. The time dimension.
type Time = Int

-- | Integer division, rounding up.
divUp :: Int -> Int -> Int
divUp n k = (n + k - 1) `div` k
