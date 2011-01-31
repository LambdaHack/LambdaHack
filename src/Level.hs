module Level where

import qualified System.Random as R
import Control.Monad

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Map as M
import qualified Data.IntMap as IM
import Data.Set as S
import Data.List as L
import Data.Ratio
import Data.Maybe

import Geometry
import Monster
import Item
import Random
import Display

-- | Names of the dungeon levels are represented using a
-- custom data structure.
data LevelName = LambdaCave Int | Exit
  deriving (Show, Eq, Ord)

-- | Chance that a new monster is generated. Currently depends on the
-- number of monsters already present, and on the level. In the future,
-- the strength of the character and the strength of the monsters present
-- could further influence the chance, and the chance could also affect
-- which monster is generated.
monsterGenChance :: LevelName -> [Monster] -> Rnd Bool
monsterGenChance (LambdaCave n) [] = chance $ 1%25
monsterGenChance (LambdaCave n) l  = chance $ 1%((400 + (fromIntegral (L.length l) * 100) - (fromIntegral n * 50)) `max` 50)
monsterGenChance _              _  = return False

instance Binary LevelName where
  put (LambdaCave n) = put n
  get = liftM LambdaCave get

-- | Provide a textual description of a level name.
levelName :: LevelName -> String
levelName (LambdaCave n) = "The Lambda Cave " ++ show n

-- | Gives the numeric representation of the level's depth.
levelNumber :: LevelName -> Int
levelNumber (LambdaCave n) = n

-- | The complete dungeon is a map from level names to levels.
-- We usually store all but the current level in this data structure.
data Dungeon = Dungeon (M.Map LevelName Level)
  deriving Show

-- | Create a dungeon from a list of levels.
dungeon :: [Level] -> Dungeon
dungeon = Dungeon . M.fromList . L.map (\ l -> (lname l, l))

-- | Extract a level from a dungeon.
getDungeonLevel :: LevelName -> Dungeon -> (Level, Dungeon)
getDungeonLevel ln (Dungeon dng) = (dng ! ln, Dungeon (M.delete ln dng))

-- | Put a level into a dungeon.
putDungeonLevel :: Level -> Dungeon -> Dungeon
putDungeonLevel lvl (Dungeon dng) = Dungeon (M.insert (lname lvl) lvl dng)

updateDungeonLevel :: (Level -> Level) -> LevelName -> Dungeon -> Dungeon
updateDungeonLevel f ln (Dungeon dng) = Dungeon (M.adjust f ln dng)

sizeDungeon :: Dungeon -> Int
sizeDungeon (Dungeon dng) = M.size dng

instance Binary Dungeon where
  put (Dungeon dng) = put (M.elems dng)
  get = liftM dungeon get

-- | A dungeon location is a level together with a location on
-- that level.
type DungeonLoc = (LevelName, Loc)

type LMonsters = IM.IntMap Monster

data Level = Level
  { lname     :: LevelName,
    lheroes   :: LMonsters,  -- ^ all but the current selected hero on the level
    lsize     :: (Y,X),
    lmonsters :: [Monster],
    lsmell    :: SMap,
    lmap      :: LMap,
    lmeta     :: String }
  deriving Show

updateLMap :: (LMap -> LMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }

updateSMap :: (SMap -> SMap) -> Level -> Level
updateSMap f lvl = lvl { lsmell = f (lsmell lvl) }

updateMonsters :: ([Monster] -> [Monster]) -> Level -> Level
updateMonsters f lvl = lvl { lmonsters = f (lmonsters lvl) }

updateHeroes :: (LMonsters -> LMonsters) -> Level -> Level
updateHeroes f lvl = lvl { lheroes = f (lheroes lvl) }

lmEmpty :: LMonsters
lmEmpty = IM.empty

instance Binary Level where
  put (Level nm hs sz@(sy,sx) ms lsmell lmap lmeta) =
        do
          put nm
          put sz
          put ms
          put hs
          put [ lsmell ! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put [ lmap ! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put lmeta
  get = do
          nm <- get
          sz@(sy,sx) <- get
          ms <- get
          hs <- get
          xs <- get
          let lsmell = M.fromList (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] xs)
          xs <- get
          let lmap   = M.fromList (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] xs)
          lmeta <- get
          return (Level nm hs sz ms lsmell lmap lmeta)

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
             | Opening Pos
             | Floor DL
             | Unknown
             | Corridor
             | Wall Pos
             | Stairs DL VDir (Maybe DungeonLoc)
             | Door Pos (Maybe Int)  -- Nothing: open, Just 0: closed, otherwise secret
  deriving Show

instance Binary Terrain where
  put Rock            = putWord8 0
  put (Opening p)     = putWord8 1 >> put p
  put (Floor dl)      = putWord8 2 >> put dl
  put Unknown         = putWord8 3
  put Corridor        = putWord8 4
  put (Wall p)        = putWord8 5 >> put p
  put (Stairs dl d n) = putWord8 6 >> put dl >> put d >> put n
  put (Door p o)      = putWord8 7 >> put p >> put o
  get = do
          tag <- getWord8
          case tag of
            0 -> return Rock
            1 -> liftM Opening get
            2 -> liftM Floor get
            3 -> return Unknown
            4 -> return Corridor
            5 -> liftM Wall get
            6 -> liftM3 Stairs get get get
            7 -> liftM2 Door get get
            _ -> fail "no parse (Tile)"

data DL = Dark | Light
  deriving (Eq, Show, Bounded)

-- | All the wall types that are possible:
--
--     * 'UL': upper left
--
--     * 'U': upper
--
--     * 'UR': upper right
--
--     * 'L': left
--
--     * 'R': right
--
--     * 'DL': lower left
--
--     * 'D': lower
--
--     * 'DR': lower right
--
--     * 'O': lower right
--
-- I am tempted to add even more (T-pieces and crossings),
-- but currently, we don't need them.
data Pos = UL | U | UR | L | R | DL | D | DR | O
  deriving (Eq, Show, Bounded)

instance Binary Pos where
  put UL = putWord16le 0
  put U  = putWord16le 1
  put UR = putWord16le 2
  put L  = putWord16le 3
  put R  = putWord16le 4
  put DL = putWord16le 5
  put D  = putWord16le 6
  put DR = putWord16le 7
  put O  = putWord16le 8

  get = do
          tag <- getWord16le
          case tag of
            0 -> return UL
            1 -> return U
            2 -> return UR
            3 -> return L
            4 -> return R
            5 -> return DL
            6 -> return D
            7 -> return DR
            8 -> return O

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

instance Binary DL where
  put Dark  = put False
  put Light = put True
  get = get >>= \ b -> if b then return Light else return Dark

data VDir = Up | Down
  deriving (Eq, Show)

instance Binary VDir where
  put Up   = put True
  put Down = put False
  get = get >>= \ b -> if b then return Up else return Down

instance Eq Terrain where
  Rock == Rock = True
  Opening d == Opening d' = d == d'
  Floor l == Floor l' = l == l'
  Unknown == Unknown = True
  Corridor == Corridor = True
  Wall p == Wall p' = p == p'
  Stairs dl d t == Stairs dl' d' t' = dl == dl' && d == d' && t == t'
  Door p o == Door p' o' = p == p' && o == o'
  _ == _ = False

-- | blocks moves and vision
closed :: Tile -> Bool
closed = not . open

floor :: Tile -> Bool
floor (Tile { tterrain = Floor _ }) = True
floor _                             = False

secret :: Maybe Int -> Bool
secret (Just n) | n /= 0 = True
secret _ = False

toOpen :: Bool -> Maybe Int
toOpen True = Nothing
toOpen False = Just 0

fromDL :: DL -> Bool
fromDL Dark = False
fromDL Light = True

toDL :: Bool -> DL
toDL False = Dark
toDL True  = Light

-- | allows moves and vision
open :: Tile -> Bool
open (Tile (Floor {}) _)     = True
open (Tile (Opening {}) _)   = True
open (Tile (Door _ o) _)     = isNothing o
open (Tile Corridor _)       = True
open (Tile (Stairs {}) _)    = True
open _                       = False

-- | is lighted on its own
light :: Tile -> Bool
light (Tile (Floor l) _)        = fromDL l
light (Tile (Stairs l _ _) _)   = fromDL l
light _                         = False

-- | can be lighted by sourrounding tiles
reflects :: Tile -> Bool
reflects (Tile (Opening _) _) = True
reflects (Tile (Wall _) _)    = True
reflects (Tile (Door _ _) _)  = True
reflects _                    = False

-- | Passive tiles reflect light from some other (usually adjacent)
-- positions. This function returns the offsets from which light is
-- reflected. Not all passively lighted tiles reflect from all directions.
-- Walls, for instance, cannot usually be seen from the outside.
passive :: Tile -> [Dir]
passive (Tile (Wall p) _)          = posToDir p
passive (Tile (Opening _) _)       = moves
passive (Tile (Door p Nothing) _)  = moves
passive (Tile (Door p (Just 0)) _) = moves
                                     -- doors can be seen from all sides
passive (Tile (Door p (Just n)) _) = posToDir p
                                     -- secret doors are like walls
passive (Tile (Stairs _ _ _) _)    = moves
passive _                          = []

-- | Perceptible is similar to passive, but describes which tiles can
-- be seen from which adjacent fields in the dark.
perceptible :: Tile -> [Dir]
perceptible (Tile Rock _) = []
perceptible p = case passive p of
                 [] -> moves
                 ds -> ds

-- | Maps wall types to lists of expected floor positions.
posToDir :: Pos -> [Dir]
posToDir UL = [downright]
posToDir U  = [down]
posToDir UR = [downleft]
posToDir L  = [right]
posToDir R  = [left]
posToDir DL = [upright]
posToDir D  = [up]
posToDir DR = [upleft]
posToDir O  = moves

-- checks for the presence of monsters (and items); it does *not* check
-- if the tile is open ...
unoccupied :: [Monster] -> LMap -> Loc -> Bool
unoccupied monsters _lmap loc =
  all (\ m -> mloc m /= loc) monsters

-- check whether one location is accessible from the other
-- precondition: the two locations are next to each other
-- currently only implements that doors aren't accessible diagonally,
-- and that the target location has to be open
accessible :: LMap -> Loc -> Loc -> Bool
accessible lmap source target =
  let dir = shift source (neg target)
      src = lmap `at` source
      tgt = lmap `at` target
  in  open tgt &&
      (not (diagonal dir) ||
       case (tterrain src, tterrain tgt) of
         (Door {}, _)  -> False
         (_, Door {})  -> False
         _             -> True)

-- check whether the location contains a door of at most secrecy level k
openable :: Int -> LMap -> Loc -> Bool
openable k lmap target =
  let tgt = lmap `at` target
  in  case tterrain tgt of
        Door _ (Just n) -> n <= k
        _               -> False

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
    let tile = lm `at` loc
    if p loc tile
      then return loc
      else findLoc l p

findLocTry :: Int ->  -- try k times
              Level ->
              (Loc -> Tile -> Bool) ->  -- loop until satisfied
              (Loc -> Tile -> Bool) ->  -- only try to satisfy k times
              Rnd Loc
findLocTry k l@(Level { lsize = sz, lmap = lm }) p pTry =
  do
    loc <- locInArea ((0,0),sz)
    let tile = lm `at` loc
    if p loc tile && pTry loc tile
      then return loc
      else if k > 1
             then findLocTry (k - 1) l p pTry
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
                  let ns = neighbors ((0,0),(ny-1,nx-1)) c  -- potential new candidates
                  let nu = S.delete c unconnected  -- new unconnected
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

-- | Produces a textual description for terrain, used if no objects
-- are present.
lookTerrain :: Terrain -> String
lookTerrain (Floor _)          = "Floor."
lookTerrain Corridor           = "Corridor."
lookTerrain (Opening _)        = "An opening."
lookTerrain (Stairs _ Up _)    = "A staircase up."
lookTerrain (Stairs _ Down _)  = "A staircase down."
lookTerrain (Door _ Nothing)   = "An open door."
lookTerrain _                  = ""

-- | The parameter "n" is the level of evolution:
--
-- 0: final
-- 1: stairs added
-- 2: doors added
-- 3: corridors and openings added
-- 4: only rooms
--
-- The Bool indicates whether the loc is currently visible.
viewTerrain :: Int -> Bool -> Terrain -> (Char, Attr -> Attr)
viewTerrain n b Rock              = (' ', id)
viewTerrain n b (Opening d)
  | n <= 3                        = ('.', id)
  | otherwise                     = viewTerrain 0 b (Wall d)
viewTerrain n b (Floor Light)     = ('.', id)
viewTerrain n b (Floor Dark)      = if b then ('.', id) else (' ', id)
viewTerrain n b Unknown           = (' ', id)
viewTerrain n b Corridor
  | n <= 3                        = ('#', id)
  | otherwise                     = viewTerrain 0 b Rock
viewTerrain n b (Wall p)
  | p == O                        = ('O', id)
  | p `elem` [L, R]               = ('|', id)
  | otherwise                     = ('-', id)
viewTerrain n b (Stairs _ Up _)
  | n <= 1                        = ('<', id)
  | otherwise                     = viewTerrain 0 b (Floor Dark)
viewTerrain n b (Stairs _ Down _)
  | n <= 1                        = ('>', id)
  | otherwise                     = viewTerrain 0 b (Floor Dark)
viewTerrain n b (Door d (Just 0))
  | n <= 2                        = ('+', setFG yellow)
  | otherwise                     = viewTerrain n b (Opening d)
viewTerrain n b (Door d (Just _))
  | n <= 2                        = viewTerrain n b (Wall d)  -- secret door
  | otherwise                     = viewTerrain n b (Opening d)
viewTerrain n b (Door p Nothing)
  | n <= 2                        = (if p `elem` [L, R] then '-' else '|', setFG yellow)
  | otherwise                     = viewTerrain n b (Opening p)

viewSmell :: Int -> (Char, Attr -> Attr)
viewSmell n = let k | n > 9    = '*'
                    | n < 0    = '-'
                    | otherwise = head . show $ n
              in  (k, setFG black . setBG green)

-- TODO: Really scatter around, if more than one or location occupied.
--       Scatter randomly or not?
--       Perhaps starting in the direction opposite to the player?
scatterItems :: [Item] -> Loc -> Level -> Level
scatterItems items loc lvl@(Level { lmap = lmap }) =
  let joinItems items = foldl (\ acc i -> snd (joinItem i acc)) items
      t = lmap `at` loc
      nt = t { titems = joinItems items (titems t) }
      ntRemember = lmap `rememberAt` loc
  in  updateLMap (M.insert loc (nt, ntRemember)) lvl
