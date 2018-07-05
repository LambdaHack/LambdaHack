-- | Rectangular areas of levels and their basic operations.
module Game.LambdaHack.Server.DungeonGen.Area
  ( Area, toArea, fromArea, spanArea, trivialArea, isTrivialArea
  , insideArea, shrink, expand, sumAreas
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntSet as IS

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Content.PlaceKind (PlaceKind)

-- | The type of areas. The bottom left and the top right points.
data Area = Area X Y X Y
  deriving (Show, Eq)

-- | Checks if it's an area with at least one field.
toArea :: (X, Y, X, Y) -> Maybe Area
toArea (x0, y0, x1, y1) = if x0 <= x1 && y0 <= y1
                          then Just $ Area x0 y0 x1 y1
                          else Nothing

fromArea :: Area -> (X, Y, X, Y)
fromArea (Area x0 y0 x1 y1) = (x0, y0, x1, y1)

-- Funny thing, Trivial area, a point, has span 1 in each dimension.
spanArea :: Area -> (Point, X, Y)
spanArea (Area x0 y0 x1 y1) = (Point x0 y0, x1 - x0 + 1, y1 - y0 + 1)

trivialArea :: Point -> Area
trivialArea (Point x y) = Area x y x y

isTrivialArea :: Area -> Bool
isTrivialArea (Area x0 y0 x1 y1) = x0 == x1 && y0 == y1

-- | Checks that a point belongs to an area.
insideArea :: Point -> Area -> Bool
{-# INLINE insideArea #-}
insideArea (Point x y) (Area x0 y0 x1 y1) =
  x1 >= x && x >= x0 && y1 >= y && y >= y0

-- | Shrink the given area on all fours sides by the amount.
shrink :: Area -> Maybe Area
shrink (Area x0 y0 x1 y1) = toArea (x0 + 1, y0 + 1, x1 - 1, y1 - 1)

expand :: Area -> Area
expand (Area x0 y0 x1 y1) = Area (x0 - 1) (y0 - 1) (x1 + 1) (y1 + 1)

-- We assume the areas are adjacent.
sumAreas :: Area -> Area -> Area
sumAreas a@(Area x0 y0 x1 y1) a'@(Area x0' y0' x1' y1') =
  if | y1 == y0' -> assert (x0 == x0' && x1 == x1' `blame` (a, a')) $
       Area x0 y0 x1 y1'
     | y0 == y1' -> assert (x0 == x0' && x1 == x1' `blame` (a, a')) $
       Area x0' y0' x1' y1
     | x1 == x0' -> assert (y0 == y0' && y1 == y1' `blame` (a, a')) $
       Area x0 y0 x1' y1
     | x0 == x1' -> assert (y0 == y0' && y1 == y1' `blame` (a, a')) $
       Area x0' y0' x1 y1'
     | otherwise -> error $ "areas not adjacent" `showFailure` (a, a')

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
