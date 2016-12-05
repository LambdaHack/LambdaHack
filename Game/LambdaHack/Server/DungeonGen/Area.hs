-- | Rectangular areas of levels and their basic operations.
module Game.LambdaHack.Server.DungeonGen.Area
  ( Area, toArea, fromArea, trivialArea, grid, shrink, expand, sumAreas
  , SpecialArea(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntSet as IS

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Content.PlaceKind (PlaceKind)

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

data SpecialArea =
    SpecialArea !Area
  | SpecialFixed !Point !(GroupName PlaceKind) !Area
  deriving Show

-- | Divide uniformly a larger area into the given number of smaller areas
-- overlapping at the edges.
--
-- When a list of fixed centers (some important points inside)
-- of (non-overlapping) areas is given, incorporate those,
-- with as little disruption, as possible.
grid :: EM.EnumMap Point (GroupName PlaceKind) -> [Point] -> (X, Y) -> Area
     -> ((X, Y), EM.EnumMap Point SpecialArea)
grid fixedCenters boot (nx, ny) (Area x0 y0 x1 y1) =
  let f z0 z1 n prev (c1 : c2 : rest) =
        let len = c2 - c1 + 1
            cn = len * n `div` (z1 - z0 - 1)
        in if cn < 2
           then let mid1 = (c1 + c2) `div` 2
                    mid2 = (c1 + c2) `divUp` 2
                    mid = if mid1 - prev > 4 then mid1 else mid2
                in (prev, mid, Just c1) : f z0 z1 n mid (c2 : rest)
           else (prev, c1 + len `div` (2 * cn), Just c1)
                : [ ( c1 + len * (2 * z - 1) `div` (2 * cn)
                    , c1 + len * (2 * z + 1) `div` (2 * cn)
                    , Nothing )
                  | z <- [1 .. cn - 1] ]
                ++ f z0 z1 n (c1 + len * (2 * cn - 1) `div` (2 * cn))
                     (c2 : rest)
      f _ z1 _ prev [c1] = [(prev, z1, Just c1)]
      f _ _ _ _ [] = assert `failure` fixedCenters
      xcs = IS.toList $ IS.fromList $ map px $ EM.keys fixedCenters ++ boot
      xallCenters = zip [0..] $ f x0 x1 nx x0 xcs
      ycs = IS.toList $ IS.fromList $ map py $ EM.keys fixedCenters ++ boot
      yallCenters = zip [0..] $ f y0 y1 ny y0 ycs
  in ( (length xallCenters, length yallCenters)
     , EM.fromDistinctAscList $
         [ ( Point x y
           , case (mcx, mcy) of
               (Just cx, Just cy) ->
                 case EM.lookup (Point cx cy) fixedCenters of
                   Nothing -> SpecialArea area
                   Just placeGroup ->
                     SpecialFixed (Point cx cy) placeGroup area
               _ -> SpecialArea area )
         | (y, (cy0, cy1, mcy)) <- yallCenters
         , (x, (cx0, cx1, mcx)) <- xallCenters
         , let area = Area cx0 cy0 cx1 cy1 ] )

-- | Shrink the given area on all fours sides by the amount.
shrink :: Area -> Maybe Area
shrink (Area x0 y0 x1 y1) = toArea (x0 + 1, y0 + 1, x1 - 1, y1 - 1)

expand :: Area -> Area
expand (Area x0 y0 x1 y1) = Area (x0 - 1) (y0 - 1) (x1 + 1) (y1 + 1)

-- We assume the areas are adjacent.
sumAreas :: Area -> Area -> Area
sumAreas a@(Area x0 y0 x1 y1) a'@(Area x0' y0' x1' y1') =
  if | y1 == y0' -> assert (x0 == x0' && x1 == x1' `blame` (a, a')) $
       (Area x0 y0 x1 y1')
     | y0 == y1' -> assert (x0 == x0' && x1 == x1' `blame` (a, a')) $
       (Area x0' y0' x1' y1)
     | x1 == x0' -> assert (y0 == y0' && y1 == y1' `blame` (a, a')) $
       (Area x0 y0 x1' y1)
     | x0 == x1' -> assert (y0 == y0' && y1 == y1' `blame` (a, a')) $
       (Area x0' y0' x1 y1')
     | otherwise -> assert `failure` (a, a')

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
