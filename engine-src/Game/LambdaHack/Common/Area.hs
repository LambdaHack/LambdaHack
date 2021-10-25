-- | Rectangular areas of levels and their basic operations.
module Game.LambdaHack.Common.Area
  ( Area, toArea, fromArea, spanArea, trivialArea, isTrivialArea
  , inside, shrink, expand, middlePoint, areaInnerBorder, sumAreas, punindex
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary

import Game.LambdaHack.Common.Point
import Game.LambdaHack.Definition.Defs

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
inside :: Point -> Area -> Bool
{-# INLINE inside #-}
inside p (Area x0 y0 x1 y1) = insideP p (x0, y0, x1, y1)

-- | Shrink the given area on all fours sides by the amount.
shrink :: Area -> Maybe Area
shrink (Area x0 y0 x1 y1) = toArea (x0 + 1, y0 + 1, x1 - 1, y1 - 1)

expand :: Area -> Area
expand (Area x0 y0 x1 y1) = Area (x0 - 1) (y0 - 1) (x1 + 1) (y1 + 1)

middlePoint :: Area -> Point
middlePoint (Area x0 y0 x1 y1) = Point (x0 + (x1 - x0) `div` 2)
                                       (y0 + (y1 - y0) `div` 2)

areaInnerBorder :: Area -> [Point]
areaInnerBorder (Area x0 y0 x1 y1) =
  [ Point x y
  | x <- [x0, x1], y <- [y0..y1] ]
  ++ [ Point x y
     | x <- [x0+1..x1-1], y <- [y0, y1] ]

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

punindex :: X -> Int -> Point
{-# INLINE punindex #-}
punindex xsize n = let (py, px) = n `quotRem` xsize
                   in Point{..}

instance Binary Area where
  put (Area x0 y0 x1 y1) = do
    put x0
    put y0
    put x1
    put y1
  get = Area <$> get <*> get <*> get <*> get
