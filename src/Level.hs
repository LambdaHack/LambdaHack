module Level where

import Control.Monad

import Data.Binary
import Data.Map as M
import Data.List as L
import qualified Data.IntMap as IM

import Geometry
import GeometryRnd
import Movable
import Item
import Random
import qualified Terrain

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

type Party = IM.IntMap Movable

data Level = Level
  { lname     :: LevelName,
    lheroes   :: Party,      -- ^ all heroes on the level
    lsize     :: (Y,X),
    lmonsters :: Party,      -- ^ all monsters on the level
    lsmell    :: SMap,
    lmap      :: LMap,
    lmeta     :: String }
  deriving Show

updateLMap :: (LMap -> LMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }

updateSMap :: (SMap -> SMap) -> Level -> Level
updateSMap f lvl = lvl { lsmell = f (lsmell lvl) }

updateMonsters :: (Party -> Party) -> Level -> Level
updateMonsters f lvl = lvl { lmonsters = f (lmonsters lvl) }

updateHeroes :: (Party -> Party) -> Level -> Level
updateHeroes f lvl = lvl { lheroes = f (lheroes lvl) }

emptyParty :: Party
emptyParty = IM.empty

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
              { tterrain :: Terrain.Terrain DungeonLoc,
                titems   :: [Item] }
  deriving Show

instance Binary Tile where
  put (Tile t is) = put t >> put is
  get = liftM2 Tile get get

at         l p = fst (findWithDefault (unknown, unknown) p l)
rememberAt l p = snd (findWithDefault (unknown, unknown) p l)

unknown :: Tile
unknown = Tile Terrain.Unknown []

-- | blocks moves and vision
closed :: Tile -> Bool
closed = not . open

floor :: Tile -> Bool
floor = Terrain.isFloor . tterrain

secret :: Maybe Int -> Bool
secret (Just n) | n /= 0 = True
secret _ = False

isUnknown :: Tile -> Bool
isUnknown = Terrain.isUnknown . tterrain

toOpen :: Bool -> Maybe Int
toOpen True = Nothing
toOpen False = Just 0

-- | allows moves and vision
open :: Tile -> Bool
open = Terrain.isOpen . tterrain

-- | is lighted on its own
light :: Tile -> Bool
light = Terrain.isAlight . tterrain

-- | can be lighted by sourrounding tiles
reflects :: Tile -> Bool
reflects = Terrain.reflects . tterrain

-- | Passive tiles reflect light from some other (usually adjacent)
-- positions. This function returns the offsets from which light is
-- reflected. Not all passively lighted tiles reflect from all directions.
-- Walls, for instance, cannot usually be seen from the outside.
passive :: Tile -> [Dir]
passive = Terrain.passive . tterrain

-- | Perceptible is similar to passive, but describes which tiles can
-- be seen from which adjacent fields in the dark.
perceptible :: Tile -> [Dir]
perceptible = Terrain.perceptible . tterrain

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
         (Terrain.Door {}, _)  -> False
         (_, Terrain.Door {})  -> False
         _             -> True)

-- check whether the location contains a door of at most secrecy level k
openable :: Int -> LMap -> Loc -> Bool
openable k lmap target =
  let tgt = lmap `at` target
  in  case tterrain tgt of
        Terrain.Door _ (Just n) -> n <= k
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

-- Actually, do not scatter items around, it's too much work for the player.
dropItemsAt :: [Item] -> Loc -> Level -> Level
dropItemsAt items loc lvl@(Level { lmap = lmap }) =
  let joinItems items = L.foldl' (\ acc i -> snd (joinItem i acc)) items
      t = lmap `at` loc
      nt = t { titems = joinItems items (titems t) }
      ntRemember = lmap `rememberAt` loc
  in  updateLMap (M.insert loc (nt, ntRemember)) lvl
