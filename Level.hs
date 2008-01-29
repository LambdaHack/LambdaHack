module Level where

import System.Random

import Data.Map as M
import Data.Set as S
import Data.List as L

type X = Int
type Y = Int

data Level = Level
              { lsize :: (Y,X),
                lmap  :: LMap }

type LMap = Map (Y,X) Tile

data Tile = Rock
          | Floor
          | Unknown
          | Corridor
          | Stairs VDir (Maybe (Level, Loc))

data VDir = Up | Down
  deriving Eq

instance Eq Tile where
  Rock == Rock = True
  Floor == Floor = True
  Unknown == Unknown = True
  Corridor == Corridor = True
  Stairs d _ == Stairs d' _ = d == d'
  _ == _ = False

instance Show Tile where
  show Rock            = "#"
  show Floor           = "."
  show Unknown         = " "
  show Corridor        = "_"
  show (Stairs Up _)   = "<"
  show (Stairs Down _) = ">"

closed, open, light :: Tile -> Bool
closed = not . open
open Floor = True
open Corridor = True
open (Stairs _ _) = True
open _ = False
light Floor = True
light (Stairs _ _) = True
light _ = False

type Loc = (Y,X)
type Area = ((Y,X),(Y,X))

locInArea :: Area -> IO Loc
locInArea ((y0,x0),(y1,x1)) =
  do
    rx <- randomRIO (x0,x1)
    ry <- randomRIO (y0,y1)
    return (ry,rx)

findLoc :: Level -> (Loc -> Tile -> Bool) -> IO Loc
findLoc l@(Level { lsize = sz, lmap = lm }) p =
  do
    loc <- locInArea ((0,0),sz)
    if p loc (findWithDefault Unknown loc lm) then return loc
                                              else findLoc l p

distance :: (Loc,Loc) -> Int
distance ((y0,x0),(y1,x1)) = (y1 - y0)^2 + (x1 - x0)^2

adjacent :: Loc -> Loc -> Bool
adjacent s t = distance (s,t) <= 2

shift :: Loc -> Loc -> Loc
shift (y0,x0) (y1,x1) = (y0+y1,x0+x1)

grid :: (Y,X) -> Area -> Map (Y,X) Area
grid (ny,nx) ((y0,x0),(y1,x1)) =
  let yd = y1 - y0
      xd = x1 - x0
  in M.fromList [ ((y,x), ((y0 + (yd * y `div` ny), x0 + (xd * x `div` nx)),
                           (y0 + (yd * (y + 1) `div` ny - 1), x0 + (xd * (x + 1) `div` nx - 1))))
                | x <- [0..nx-1], y <- [0..ny-1] ]


connectGrid :: (Y,X) -> IO [((Y,X),(Y,X))]
connectGrid (ny,nx) =
  do
    let unconnected = S.fromList [ (y,x) | x <- [0..nx-1], y <- [0..ny-1] ]
    -- candidates are neighbors that are still unconnected; we start with
    -- a random choice
    rx <- randomRIO (0,nx-1)
    ry <- randomRIO (0,ny-1)
    let candidates  = S.fromList [ (ry,rx) ]
    connectGrid' (ny,nx) unconnected candidates []

connectGrid' :: (Y,X) -> Set (Y,X) -> Set (Y,X) -> [((Y,X),(Y,X))] -> IO [((Y,X),(Y,X))]
connectGrid' (ny,nx) unconnected candidates acc
  | S.null candidates = return acc
  | otherwise = do
                  r <- randomRIO (0,S.size candidates - 1)
                  let c = S.toList candidates !! r
                  let ns = neighbors ((0,0),(ny-1,nx-1)) c -- potential new candidates
                  let nu = S.delete c unconnected -- new unconnected
                  let (nc,ds) = S.partition (`S.member` nu) ns
                                  -- (new candidates, potential connections)
                  new <- if S.null ds then return id
                                      else do
                                             s <- randomRIO (0,S.size ds - 1)
                                             let d = S.toList ds !! s
                                             return ((c,d) :)
                  connectGrid' (ny,nx) nu
                                       (S.delete c (candidates `S.union` nc)) (new acc)

neighbors :: Area ->        {- size limitation -}
             Loc ->         {- location to find neighbors of -}
             Set Loc
neighbors area (y,x) =
  let cs = [ (y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1], (dx + dy) `mod` 2 == 1 ] 
  in  S.fromList (L.filter (`inside` area) cs)

inside :: Loc -> Area -> Bool
inside (y,x) ((y0,x0),(y1,x1)) = x1 >= x && x >= x0 && y1 >= y && y >= y0


fromTo :: Loc -> Loc -> [Loc]
fromTo (y0,x0) (y1,x1)
  | y0 == y1 = L.map (\ x -> (y0,x)) (fromTo1 x0 x1)
  | x0 == x1 = L.map (\ y -> (y,x0)) (fromTo1 y0 y1)

fromTo1 :: X -> X -> [X]
fromTo1 x0 x1
  | x0 <= x1  = [x0..x1]
  | otherwise = [x0,x0-1..x1]


