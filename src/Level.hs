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
import Data.Maybe

import Geometry
import GeometryRnd
import Movable
import Item
import qualified Attr
import Random

-- | Names of the dungeon levels are represented using a
-- custom data structure.
data LevelName = LambdaCave Int | Exit
  deriving (Show, Eq, Ord)

instance Binary LevelName where
  put (LambdaCave n) = put n
  get = liftM LambdaCave get

-- | Provide a textual description of a level name.
levelName :: LevelName -> String
levelName (LambdaCave n) = "The Lambda Cave " ++ show n

-- | Gives the numeric representation of the level's depth.
levelNumber :: LevelName -> Int
levelNumber (LambdaCave n) = n

-- | A dungeon location is a level together with a location on that level.
type DungeonLoc = (LevelName, Loc)

type LMovables = IM.IntMap Movable

data Level = Level
  { lname     :: LevelName,
    lheroes   :: LMovables,  -- ^ all but the current selected hero on the level
    lsize     :: (Y,X),
    lmonsters :: LMovables,  -- ^ all monsters on the level
    lsmell    :: SMap,
    lmap      :: LMap,
    lmeta     :: String }
  deriving Show

updateLMap :: (LMap -> LMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }

updateSMap :: (SMap -> SMap) -> Level -> Level
updateSMap f lvl = lvl { lsmell = f (lsmell lvl) }

updateMonsters :: (LMovables -> LMovables) -> Level -> Level
updateMonsters f lvl = lvl { lmonsters = f (lmonsters lvl) }

updateHeroes :: (LMovables -> LMovables) -> Level -> Level
updateHeroes f lvl = lvl { lheroes = f (lheroes lvl) }

lmEmpty :: LMovables
lmEmpty = IM.empty

instance Binary Level where
  put (Level nm hs sz@(sy,sx) ms lsmell lmap lmeta) =
        do
          put nm
          put hs
          put sz
          put ms
          put [ lsmell ! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put [ lmap ! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put lmeta
  get = do
          nm <- get
          hs <- get
          sz@(sy,sx) <- get
          ms <- get
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
  randomR (a,b) g = case R.randomR (fromHV a, fromHV b) g of
                      (b, g') -> (toHV b, g')
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

isUnknown :: Tile -> Bool
isUnknown (Tile Unknown []) = True
isUnknown _                 = False

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

-- Checks for the presence of movables. Does *not* check if the tile is open.
unoccupied :: [Movable] -> Loc -> Bool
unoccupied movables loc =
  all (\ m -> mloc m /= loc) movables

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


-- | Produces a textual description for terrain, used if no objects
-- are present.
lookTerrain :: Terrain -> String
lookTerrain (Floor _)          = "Floor."
lookTerrain Corridor           = "Corridor."
lookTerrain (Opening _)        = "An opening."
lookTerrain (Stairs _ Up _)    = "A staircase up."
lookTerrain (Stairs _ Down _)  = "A staircase down."
lookTerrain (Door _ Nothing)   = "An open door."
lookTerrain (Door _ (Just 0))  = "A closed door."
lookTerrain (Door _ (Just _))  = "A wall."  -- secret
lookTerrain (Wall _ )          = "A wall."
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
viewTerrain :: Int -> Bool -> Terrain -> (Char, Attr.Color)
viewTerrain n b t =
  let def =     if b then Attr.BrWhite else Attr.defFG
      defDark = if b then Attr.BrYellow else Attr.BrBlack
      defDoor = if b then Attr.Yellow else Attr.BrBlack
  in case t of
       Rock                -> (' ', def)
       (Opening d)
         | n <= 3          -> ('.', def)
         | otherwise       -> viewTerrain 0 b (Wall d)
       (Floor d)           -> ('.', if d == Light then def else defDark)
       Unknown             -> (' ', def)
       Corridor
         | n <= 3          -> ('#', if b then Attr.BrWhite else Attr.defFG)
         | otherwise       -> viewTerrain 0 b Rock
       (Wall p)
         | p == O          -> ('O', def)
         | p `elem` [L, R] -> ('|', def)
         | otherwise       -> ('-', def)
       (Stairs d p _)
         | n <= 1          -> (if p == Up then '<' else '>',
                               if d == Light then def else defDark)
         | otherwise       -> viewTerrain 0 b (Floor Dark)
       (Door p (Just 0))
         | n <= 2          -> ('+', defDoor)
         | otherwise       -> viewTerrain n b (Opening p)
       (Door p (Just _))
         | n <= 2          -> viewTerrain n b (Wall p)  -- secret door
         | otherwise       -> viewTerrain n b (Opening p)
       (Door p Nothing)
         | n <= 2          -> (if p `elem` [L, R] then '-' else '|', defDoor)
         | otherwise       -> viewTerrain n b (Opening p)

-- Actually, do not scatter the items around, it's too much work for the player.
scatterItems :: [Item] -> Loc -> Level -> Level
scatterItems items loc lvl@(Level { lmap = lmap }) =
  let joinItems items = foldl' (\ acc i -> snd (joinItem i acc)) items
      t = lmap `at` loc
      nt = t { titems = joinItems items (titems t) }
      ntRemember = lmap `rememberAt` loc
  in  updateLMap (M.insert loc (nt, ntRemember)) lvl
