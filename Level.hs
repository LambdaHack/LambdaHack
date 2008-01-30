module Level where

import System.Random
import Control.Monad

import Data.Binary
import Data.Map as M
import Data.Set as S
import Data.List as L

type X = Int
type Y = Int

data Level = Level
              { lname :: String,
                lsize :: (Y,X),
                lmap  :: LMap }
  deriving Show

instance Binary Level where
  put (Level nm sz@(sy,sx) lmap) = put nm >> put sz >> put [ lmap ! (y,x) | y <- [0..sy], x <- [0..sx] ]
  get = do
          nm <- get
          sz@(sy,sx) <- get
          xs <- get
          let lmap = M.fromList (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] xs)
          return (Level nm sz lmap)

type LMap = Map (Y,X) (Tile,Tile)

at         l p = fst (findWithDefault (Unknown, Unknown) p l)
rememberAt l p = snd (findWithDefault (Unknown, Unknown) p l)

data Tile = Rock
          | Opening
          | Floor
          | Unknown
          | Corridor
          | Wall HV
          | Stairs VDir (Maybe (Level, Loc))
  deriving Show

-- forget stuff you cannot see anyway
-- mainly for space efficiency in save files
flat :: Tile -> Tile
flat (Stairs d _) = Stairs d Nothing
flat x            = x

instance Binary Tile where
  put Rock         = putWord8 0
  put Opening      = putWord8 1
  put Floor        = putWord8 2
  put Unknown      = putWord8 3
  put Corridor     = putWord8 4
  put (Wall d)     = putWord8 5 >> put d
  put (Stairs d n) = putWord8 6 >> put d >> put n
  get = do
          tag <- getWord8
          case tag of
            0 -> return Rock
            1 -> return Opening
            2 -> return Floor
            3 -> return Unknown
            4 -> return Corridor
            5 -> liftM Wall get
            6 -> liftM2 Stairs get get

data HV = Horiz | Vert
  deriving (Eq, Show)

instance Binary HV where
  put Horiz = put True
  put Vert  = put False
  get = get >>= \ b -> if b then return Horiz else return Vert

data VDir = Up | Down
  deriving (Eq, Show)

instance Binary VDir where
  put Up   = put True
  put Down = put False
  get = get >>= \ b -> if b then return Up else return Down

instance Eq Tile where
  Rock == Rock = True
  Opening == Opening = True
  Floor == Floor = True
  Unknown == Unknown = True
  Corridor == Corridor = True
  Wall d == Wall d' = d == d'
  Stairs d _ == Stairs d' _ = d == d'
  _ == _ = False

view :: Tile -> Char
view Rock            = ' '
view Opening         = '.'
view Floor           = '.'
view Unknown         = ' '
view Corridor        = '#'
view (Wall Horiz)    = '-'
view (Wall Vert)     = '|'
view (Stairs Up _)   = '<'
view (Stairs Down _) = '>'

closed, open, light :: Tile -> Bool
closed = not . open
open Floor = True
open Opening = True
open Corridor = True
open (Stairs _ _) = True
open _ = False
light Floor = True
light Opening = True
light (Stairs _ _) = True
light (Wall _) = True
light _ = False

type Loc = (Y,X)
type Area = ((Y,X),(Y,X))

findLocInArea :: Area -> (Loc -> Bool) -> IO Loc
findLocInArea a@((y0,x0),(y1,x1)) p =
  do
    rx <- randomRIO (x0,x1)
    ry <- randomRIO (y0,y1)
    let loc = (ry,rx)
    if p loc then return loc else findLocInArea a p

locInArea :: Area -> IO Loc
locInArea a = findLocInArea a (const True)

findLoc :: Level -> (Loc -> Tile -> Bool) -> IO Loc
findLoc l@(Level { lsize = sz, lmap = lm }) p =
  do
    loc <- locInArea ((0,0),sz)
    if p loc (lm `at` loc) then return loc
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

randomConnection :: (Y,X) -> IO ((Y,X),(Y,X))
randomConnection (ny,nx) =
  do
    rb  <- randomRIO (False,True)
    if rb then do
                 rx  <- randomRIO (0,nx-2)
                 ry  <- randomRIO (0,ny-1)
                 return (normalize ((ry,rx),(ry,rx+1)))
          else do
                 ry  <- randomRIO (0,ny-2)
                 rx  <- randomRIO (0,nx-1)
                 return (normalize ((ry,rx),(ry+1,rx)))

normalize :: ((Y,X),(Y,X)) -> ((Y,X),(Y,X))
normalize (a,b) | a <= b    = (a,b)
                | otherwise = (b,a)

connectGrid' :: (Y,X) -> Set (Y,X) -> Set (Y,X) -> [((Y,X),(Y,X))] -> IO [((Y,X),(Y,X))]
connectGrid' (ny,nx) unconnected candidates acc
  | S.null candidates = return (L.map normalize acc)
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


