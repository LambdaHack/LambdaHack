-- | Rectangular areas of levels and their basic operations.
module Game.LambdaHack.Server.DungeonGen.Area
  ( Area, toArea, fromArea, trivialArea, grid, shrink
  ) where

import Data.Binary

import Game.LambdaHack.Common.Point

-- | The type of areas. The bottom left and the top right points.
data Area = Area !X !Y !X !Y
  deriving Show

-- | Checks if it's an area with at least one field.
toArea :: (X, Y, X, Y) -> Maybe Area
toArea (x0, y0, x1, y1) = if x0 <= x1 && y0 <= y1
                          then Just $ Area x0 y0 x1 y1
                          else Nothing

fromArea :: Area -> (X, Y, X, Y)
fromArea (Area x0 y0 x1 y1) = (x0, y0, x1, y1)

trivialArea :: Point -> Area
trivialArea (Point x y) = Area x y x y

-- | Divide uniformly a larger area into the given number of smaller areas
-- overlapping at the edges.
grid :: (X, Y) -> Area -> [(Point, Area)]
grid (nx, ny) (Area x0 y0 x1 y1) =
  let xd = x1 - x0  -- not +1, because we need overlap
      yd = y1 - y0
  in [ (Point x y, Area (x0 + xd * x `div` nx)
                          (y0 + yd * y `div` ny)
                          (x0 + xd * (x + 1) `div` nx)
                          (y0 + yd * (y + 1) `div` ny))
     | x <- [0..nx-1], y <- [0..ny-1] ]

-- | Enlarge (or shrink) the given area on all fours sides by the amount.
shrink :: Area -> Maybe Area
shrink (Area x0 y0 x1 y1) = toArea (x0 + 1, y0 + 1, x1 - 1, y1 - 1)

instance Binary Area where
  put (Area x0 y0 x1 y1) = do
    put x0
    put y0
    put x1
    put y1
  get = do
    x0 <- get
    y0 <- get
    x1 <- get
    y1 <- get
    return (Area x0 y0 x1 y1)
