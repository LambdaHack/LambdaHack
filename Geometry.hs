module Geometry where

-- | Game time in turns. (Placement in module Geometry is not ideal.)
type Time = Int

type X = Int
type Y = Int

type Loc = (Y,X)
type Dir = (Y,X)
type Area = ((Y,X),(Y,X))

distance :: (Loc,Loc) -> Int
distance ((y0,x0),(y1,x1)) = (y1 - y0)^2 + (x1 - x0)^2

adjacent :: Loc -> Loc -> Bool
adjacent s t = distance (s,t) <= 2

diagonal :: Loc -> Bool
diagonal (y,x) = y*x /= 0

shift :: Loc -> Dir -> Loc
shift (y0,x0) (y1,x1) = (y0+y1,x0+x1)

neg :: Dir -> Dir
neg (y,x) = (-y,-x)

moves :: [Dir]
moves = [ (x,y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]
