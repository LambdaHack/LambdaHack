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
import WorldLoc

type Party = IM.IntMap Movable

data Level = Level
  { lname     :: LevelId,    -- TODO: remove
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
              { tterrain :: Terrain.Terrain WorldLoc,
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

canBeDoor :: Tile -> Bool
canBeDoor t =
  case t of
    Tile d@(Terrain.Door o) _ | secret o -> True
    _ ->
      Terrain.isWall (tterrain t) ||
      Terrain.isRock (tterrain t) ||
      Terrain.isUnknown (tterrain t)

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

-- | marks an exit from a room
isExit :: Tile -> Bool
isExit = Terrain.isExit . tterrain

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

-- check whether the location contains a door of secrecy level lower than k
openable :: Int -> LMap -> Loc -> Bool
openable k lmap target =
  let tgt = lmap `at` target
  in  case tterrain tgt of
        Terrain.Door (Just n) -> n < k
        _                     -> False

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
