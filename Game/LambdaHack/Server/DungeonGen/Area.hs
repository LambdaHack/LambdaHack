-- | Rectangular areas of levels and their basic operations.
module Game.LambdaHack.Server.DungeonGen.Area
  ( Area, toArea, fromArea, trivialArea, grid, shrink
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.IntSet as IS

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
-- overlapping at the edges. The list is in ascending Enum-order of points.
--
-- When a list of fixed centers (some important points inside)
-- of (non-overlapping) areas is given, incorporate those,
-- with as little disruption, as possible.
grid :: [Point] -> (X, Y) -> Area -> ((X, Y), [(Point, Area)])
grid fixedCenters (nx, ny) (Area x0 y0 x1 y1) =
  let f z0 z1 n prev (c1 : c2 : rest) =
        let len = c2 - c1 + 1
            cn = len * n `div` (z1 - z0 - 1)
        in if cn < 2
           then let middle = (c1 + c2) `div` 2
                in (prev, middle) : f z0 z1 n middle (c2 : rest)
           else (prev, c1 + len `div` (2 * cn))
                : [ ( c1 + len * (2 * z - 1) `div` (2 * cn)
                    , c1 + len * (2 * z + 1) `div` (2 * cn) )
                  | z <- [1 .. cn - 1] ]
                ++ f z0 z1 n (c1 + len * (2 * cn - 1) `div` (2 * cn))
                     (c2 : rest)
      f _ z1 _ prev _ = [(prev, z1)]
      xcs = IS.toList $ IS.fromList $ map px fixedCenters
      xallCenters = zip [0..] $ f x0 x1 nx x0 xcs
      ycs = IS.toList $ IS.fromList $ map py fixedCenters
      yallCenters = zip [0..] $ f y0 y1 ny y0 ycs
  in ( (length xallCenters, length yallCenters)
     , [ (Point x y, Area cx0 cy0 cx1 cy1)
       | (y, (cy0, cy1)) <- yallCenters
       , (x, (cx0, cx1)) <- xallCenters ] )

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
