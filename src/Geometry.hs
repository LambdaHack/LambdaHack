module Geometry where

-- | Game time in turns. (Placement in module Geometry is not ideal.)
type Time = Int

type X = Int
type Y = Int

type Loc  = (Y,X)
type Dir  = (Y,X)
type Area = ((Y,X),(Y,X))

-- | Given two locations, determine the direction in which one should
-- move from the first in order to get closer to the second. Does not
-- pay attention to obstacles at all.
towards :: (Loc,Loc) -> Dir
towards ((y0,x0),(y1,x1)) =
  let dy = y1 - y0
      dx = x1 - x0
      angle = atan (fromIntegral dy / fromIntegral dx) / (pi / 2)
      dir | angle <= -0.75 = (-1,0)
          | angle <= -0.25 = (-1,1)
          | angle <= 0.25  = (0,1)
          | angle <= 0.75  = (1,1)
          | angle <= 1.25  = (1,0)
          | otherwise      = (0,0)
  in  if dx >= 0 then dir else neg dir

-- | Get the squared distance between two locations.
distance :: (Loc,Loc) -> Int
distance ((y0,x0),(y1,x1)) = (y1 - y0)^2 + (x1 - x0)^2

-- | Return whether two locations are adjacent on the map
-- (horizontally, vertically or diagonally). Currrently, a
-- position is also considered adjacent to itself.
adjacent :: Loc -> Loc -> Bool
adjacent s t = distance (s,t) <= 2

-- | Return the 8 surrounding locations of a given location.
surroundings :: Loc -> [Loc]
surroundings l = map (l `shift`) moves

diagonal :: Dir -> Bool
diagonal (y,x) = y*x /= 0

-- | Move one square in the given direction.
shift :: Loc -> Dir -> Loc
shift (y0,x0) (y1,x1) = (y0+y1,x0+x1)

-- | Invert a direction (vector).
neg :: Dir -> Dir
neg (y,x) = (-y,-x)

-- | Get the vectors of all the moves.
moves :: [Dir]
moves = [ (x,y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]

up, down, left, right :: Dir
upleft, upright, downleft, downright :: Dir
upleft    = up   `shift` left
upright   = up   `shift` right
downleft  = down `shift` left
downright = down `shift` right
up        = (-1,0)
down      = (1,0)
left      = (0,-1)
right     = (0,1)

horiz, vert :: [Dir]
horiz = [left, right]
vert  = [up, down]
