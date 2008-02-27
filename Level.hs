module Level where

import qualified System.Random as R
import Control.Monad

import Data.Binary
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Ratio
import Data.Maybe

import Geometry
import Monster
import Item
import State
import Random
import Display

data Level = Level
              { lname     :: String,
                lsize     :: (Y,X),
                lmonsters :: [Monster],
                lsmell    :: SMap,
                lmap      :: LMap,
                lmeta     :: String }
  deriving Show

updateLMap :: Level -> (LMap -> LMap) -> Level
updateLMap lvl f = lvl { lmap = f (lmap lvl) }

updateMonsters :: Level -> ([Monster] -> [Monster]) -> Level
updateMonsters lvl f = lvl { lmonsters = f (lmonsters lvl) }

instance Binary Level where
  put (Level nm sz@(sy,sx) ms lsmell lmap lmeta) = 
        do
          put nm
          put sz
          put ms
          put [ lsmell ! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put [ lmap ! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put lmeta
  get = do
          nm <- get
          sz@(sy,sx) <- get
          ms <- get
          xs <- get
          let lsmell = M.fromList (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] xs)
          xs <- get
          let lmap   = M.fromList (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] xs)
          lmeta <- get
          return (Level nm sz ms lsmell lmap lmeta)

type LMap = Map (Y,X) (Tile,Tile)
type SMap = Map (Y,X) Time

data Tile = Tile
              { tterrain :: Terrain,
                titems   :: [Item] }
  deriving Show

instance Binary Tile where
  put (Tile t is) = put t >> put is
  get = liftM2 Tile get get

at         l p = fst (findWithDefault (unknown, unknown) p l)
rememberAt l p = snd (findWithDefault (unknown, unknown) p l)

unknown :: Tile
unknown = Tile Unknown []

data Terrain = Rock
             | Opening HV
             | Floor
             | Unknown
             | Corridor
             | Wall HV
             | Stairs VDir (Maybe (Level, Loc))
             | Door HV (Maybe Int) -- Nothing: open, Just 0: closed, otherwise secret
  deriving Show

-- forget stuff you cannot see anyway
-- mainly for space efficiency in save files
flat :: Tile -> Tile
flat (Tile (Stairs d _) is) = Tile (Stairs d Nothing) is
flat x                      = x

instance Binary Terrain where
  put Rock         = putWord8 0
  put (Opening d)  = putWord8 1 >> put d
  put Floor        = putWord8 2
  put Unknown      = putWord8 3
  put Corridor     = putWord8 4
  put (Wall d)     = putWord8 5 >> put d
  put (Stairs d n) = putWord8 6 >> put d >> put n
  put (Door d o)   = putWord8 7 >> put d >> put o
  get = do
          tag <- getWord8
          case tag of
            0 -> return Rock
            1 -> liftM Opening get
            2 -> return Floor
            3 -> return Unknown
            4 -> return Corridor
            5 -> liftM Wall get
            6 -> liftM2 Stairs get get
            7 -> liftM2 Door get get
            _ -> fail "no parse (Tile)"

data HV = Horiz | Vert
  deriving (Eq, Show, Bounded)

fromHV Horiz = True
fromHV Vert  = False

toHV True  = Horiz
toHV False = Vert

instance R.Random HV where
  randomR (a,b) g = case R.randomR (fromHV a,fromHV b) g of
                      (b,g') -> (toHV b,g')
  random g = R.randomR (minBound, maxBound) g

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

instance Eq Terrain where
  Rock == Rock = True
  Opening d == Opening d' = d == d'
  Floor == Floor = True
  Unknown == Unknown = True
  Corridor == Corridor = True
  Wall d == Wall d' = d == d'
  Stairs d _ == Stairs d' _ = d == d'
  Door d o == Door d' o' = d == d' && o == o'
  _ == _ = False

-- | blocks moves and vision
closed :: Tile -> Bool
closed = not . open

secret :: Maybe Int -> Bool
secret (Just n) | n /= 0 = True
secret _ = False

toOpen :: Bool -> Maybe Int
toOpen True = Nothing
toOpen False = Just 0

-- | allows moves and vision
open :: Tile -> Bool
open (Tile Floor _) = True
open (Tile (Opening _) _)  = True
open (Tile (Door _ o) _)   = isNothing o
open (Tile Corridor _)     = True
open (Tile (Stairs _ _) _) = True
open _                     = False

-- | is lighted on its own
light :: Tile -> Bool
light (Tile Floor _)            = True
light (Tile (Opening _) _)      = True
light (Tile (Door _ Nothing) _) = True -- open doors are all visible currently
light (Tile (Stairs _ _) _)     = True
light (Tile (Wall _) _)         = False
light _                         = False

-- | reflects light from adjacent positions;
-- exclusively passive: cannot be seen from an adjacent position in darkness
passive :: Tile -> (Bool,[(Y,X)])  -- exclusively passive?
passive (Tile (Wall Horiz) _) = (True, vert ++ [(-1,1),(1,1),(-1,-1),(1,-1)]) -- for corners
passive (Tile (Wall Vert) _)  = (True, horiz)
passive (Tile (Door Horiz (Just 0)) _) = (False, vert)
passive (Tile (Door Vert (Just 0)) _)  = (False, horiz)
passive (Tile (Door Horiz (Just n)) _) = (True, vert)
passive (Tile (Door Vert (Just n)) _)  = (True, horiz)
passive _                     = (False, [])

-- checks for the presence of monsters (and items); it does *not* check
-- if the tile is open ...
unoccupied :: [Monster] -> LMap -> Loc -> Bool
unoccupied monsters lvl loc =
  all (\ m -> mloc m /= loc) monsters

-- check whether one location is accessible from the other
-- precondition: the two locations are next to each other
-- currently only implements that doors aren't accessible diagonally,
-- and that the target location has to be open
accessible :: LMap -> Loc -> Loc -> Bool
accessible lvl source target =
  let dir = shift source (neg target)
      src = lvl `at` source
      tgt = lvl `at` target
  in  open tgt &&
      (not (diagonal dir) || 
       case (tterrain src, tterrain tgt) of
         (Door _ _, _) -> False
         (_, Door _ _) -> False
         _             -> True)

horiz = [(0,-1),(0,1)]
vert  = [(-1,0),(1,0)]

findLocInArea :: Area -> (Loc -> Bool) -> Rnd Loc
findLocInArea a@((y0,x0),(y1,x1)) p =
  do
    rx <- randomR (x0,x1)
    ry <- randomR (y0,y1)
    let loc = (ry,rx)
    if p loc then return loc else findLocInArea a p

locInArea :: Area -> Rnd Loc
locInArea a = findLocInArea a (const True)

findLoc :: Level -> (Loc -> Tile -> Bool) -> Rnd Loc
findLoc l@(Level { lsize = sz, lmap = lm }) p =
  do
    loc <- locInArea ((0,0),sz)
    if p loc (lm `at` loc) then return loc
                           else findLoc l p

grid :: (Y,X) -> Area -> Map (Y,X) Area
grid (ny,nx) ((y0,x0),(y1,x1)) =
  let yd = y1 - y0
      xd = x1 - x0
  in M.fromList [ ((y,x), ((y0 + (yd * y `div` ny), x0 + (xd * x `div` nx)),
                           (y0 + (yd * (y + 1) `div` ny - 1), x0 + (xd * (x + 1) `div` nx - 1))))
                | x <- [0..nx-1], y <- [0..ny-1] ]


connectGrid :: (Y,X) -> Rnd [((Y,X),(Y,X))]
connectGrid (ny,nx) =
  do
    let unconnected = S.fromList [ (y,x) | x <- [0..nx-1], y <- [0..ny-1] ]
    -- candidates are neighbors that are still unconnected; we start with
    -- a random choice
    rx <- randomR (0,nx-1)
    ry <- randomR (0,ny-1)
    let candidates  = S.fromList [ (ry,rx) ]
    connectGrid' (ny,nx) unconnected candidates []

randomConnection :: (Y,X) -> Rnd ((Y,X),(Y,X))
randomConnection (ny,nx) =
  do
    rb  <- randomR (False,True)
    if rb then do
                 rx  <- randomR (0,nx-2)
                 ry  <- randomR (0,ny-1)
                 return (normalize ((ry,rx),(ry,rx+1)))
          else do
                 ry  <- randomR (0,ny-2)
                 rx  <- randomR (0,nx-1)
                 return (normalize ((ry,rx),(ry+1,rx)))

normalize :: ((Y,X),(Y,X)) -> ((Y,X),(Y,X))
normalize (a,b) | a <= b    = (a,b)
                | otherwise = (b,a)

normalizeArea :: Area -> Area
normalizeArea a@((y0,x0),(y1,x1)) = ((min y0 y1, min x0 x1), (max y0 y1, max x0 x1))

connectGrid' :: (Y,X) -> Set (Y,X) -> Set (Y,X) -> [((Y,X),(Y,X))] -> Rnd [((Y,X),(Y,X))]
connectGrid' (ny,nx) unconnected candidates acc
  | S.null candidates = return (L.map normalize acc)
  | otherwise = do
                  c <- oneOf (S.toList candidates)
                  let ns = neighbors ((0,0),(ny-1,nx-1)) c -- potential new candidates
                  let nu = S.delete c unconnected -- new unconnected
                  let (nc,ds) = S.partition (`S.member` nu) ns
                                  -- (new candidates, potential connections)
                  new <- if S.null ds then return id
                                      else do
                                             d <- oneOf (S.toList ds)
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

viewTile :: Tile -> (Char, Attr -> Attr)
viewTile (Tile t [])    = viewTerrain 0 t
viewTile (Tile t (i:_)) = viewItem i

-- | Produces a textual description of the items at a location. It's
-- probably correct to use 'at' rather than 'rememberAt' at this point,
-- although we could argue that 'rememberAt' reflects what the player can
-- perceive more correctly ...
lookAt :: Bool -> LMap -> Loc -> String
lookAt detailed lvl loc
  | L.null is && detailed = lookTerrain (tterrain (lvl `at` loc))
  | otherwise             = isd
  where
    is  = titems (lvl `at` loc)
    isd = unwords $ L.map objectItem $ is


-- | Produces a textual description for terrain, used if no objects
-- are present.
lookTerrain :: Terrain -> String
lookTerrain Floor            = "empty floor"
lookTerrain Corridor         = "empty corridor"
lookTerrain (Opening _)      = "an opening"
lookTerrain (Stairs Up _)    = "staircase up"
lookTerrain (Stairs Down _)  = "staircase down"
lookTerrain (Door _ Nothing) = "an open door"
lookTerrain _                = ""

-- | The parameter "n" is the level of evolution:
--
-- 0: final
-- 1: stairs added
-- 2: doors added
-- 3: corridors and openings added
-- 4: only rooms
viewTerrain :: Int -> Terrain -> (Char, Attr -> Attr)
viewTerrain n Rock              = (' ', id)
viewTerrain n (Opening d)
  | n <= 3                      = ('.', id)
  | otherwise                   = viewTerrain 0 (Wall d)
viewTerrain n Floor             = ('.', id)
viewTerrain n Unknown           = (' ', id)
viewTerrain n Corridor
  | n <= 3                      = ('#', id)
  | otherwise                   = viewTerrain 0 Rock
viewTerrain n (Wall Horiz)      = ('-', id)
viewTerrain n (Wall Vert)       = ('|', id)
viewTerrain n (Stairs Up _)
  | n <= 1                      = ('<', id)
  | otherwise                   = viewTerrain 0 Floor
viewTerrain n (Stairs Down _)
  | n <= 1                      = ('>', id)
  | otherwise                   = viewTerrain 0 Floor
viewTerrain n (Door d (Just 0))
  | n <= 2                      = ('+', setFG yellow)
  | otherwise                   = viewTerrain n (Opening d)
viewTerrain n (Door d (Just _))
  | n <= 2                      = viewTerrain n (Wall d) -- secret door
  | otherwise                   = viewTerrain n (Opening d)
viewTerrain n (Door Horiz Nothing)
  | n <= 2                      = ('|', setFG yellow)
  | otherwise                   = viewTerrain n (Opening Horiz)
viewTerrain n (Door Vert Nothing)
  | n <= 2                      = ('-', setFG yellow)
  | otherwise                   = viewTerrain n (Opening Vert)

viewSmell :: Int -> (Char, Attr -> Attr)
viewSmell n = let k | n > 9    = '*'
                    | n < 0    = '-'
                    | otherwise = head . show $ n
              in  (k, setFG black . setBG green)

