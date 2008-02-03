module Geometry where

type X = Int
type Y = Int

type Loc = (Y,X)
type Area = ((Y,X),(Y,X))

distance :: (Loc,Loc) -> Int
distance ((y0,x0),(y1,x1)) = (y1 - y0)^2 + (x1 - x0)^2

adjacent :: Loc -> Loc -> Bool
adjacent s t = distance (s,t) <= 2

diagonal :: Loc -> Bool
diagonal (y,x) = y*x /= 0
