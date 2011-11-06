module Area
  ( Area, neighbors, fromTo, normalize, normalizeArea, grid ) where

import qualified Data.List as L

import Geometry
import Utils.Assert

type Area = (X, Y, X, Y)

neighbors :: Area ->        {- size limitation -}
             (X, Y) ->      {- location to find neighbors of -}
             [(X, Y)]
neighbors area xy =
  let cs = [ xy `shiftXY` (dx, dy)
           | dy <- [-1..1], dx <- [-1..1], (dx + dy) `mod` 2 == 1 ]
  in  L.filter (`inside` area) cs

inside :: (X, Y) -> Area -> Bool
inside (x, y) (x0, y0, x1, y1) =
  x1 >= x && x >= x0 && y1 >= y && y >= y0

fromTo :: (X, Y) -> (X, Y) -> [(X, Y)]
fromTo (x0, y0) (x1, y1) =
 let result
       | x0 == x1 = L.map (\ y -> (x0, y)) (fromTo1 y0 y1)
       | y0 == y1 = L.map (\ x -> (x, y0)) (fromTo1 x0 x1)
       | otherwise = assert `failure` ((x0, y0), (x1, y1))
 in result

fromTo1 :: Int -> Int -> [Int]
fromTo1 x0 x1
  | x0 <= x1  = [x0..x1]
  | otherwise = [x0,x0-1..x1]

normalize :: ((X, Y), (X, Y)) -> ((X, Y), (X, Y))
normalize (a, b) | a <= b    = (a, b)
                 | otherwise = (b, a)

normalizeArea :: Area -> Area
normalizeArea (x0, y0, x1, y1) = (min x0 x1, min y0 y1, max x0 x1, max y0 y1)

grid :: (X, Y) -> Area -> [((X, Y), Area)]
grid (nx, ny) (x0, y0, x1, y1) =
  let yd = y1 - y0
      xd = x1 - x0
  in [ ((x, y), (x0 + (xd * x `div` nx),
                 y0 + (yd * y `div` ny),
                 x0 + (xd * (x + 1) `div` nx - 1),
                 y0 + (yd * (y + 1) `div` ny - 1)))
     | x <- [0..nx-1], y <- [0..ny-1] ]
