module Level where

import Data.Binary
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.IntMap as IM

import Geometry
import GeometryRnd
import Actor
import Item
import Random
import WorldLoc
import Data.Maybe
import qualified Tile
import TileKind

type Party = IM.IntMap Actor

data Level = Level
  { lname     :: LevelId    -- TODO: remove
  , lheroes   :: Party      -- ^ all heroes on the level
  , lsize     :: (Y,X)
  , lmonsters :: Party      -- ^ all monsters on the level
  , lsmell    :: SMap
  , lmap      :: LMap
  , lmeta     :: String
  }
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
  put (Level nm hs sz@(sy,sx) ms ls lm lme) =
        do
          put nm
          put hs
          put sz
          put ms
          put [ ls M.! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put [ lm M.! (y,x) | y <- [0..sy], x <- [0..sx] ]
          put lme
  get = do
          nm <- get
          hs <- get
          sz@(sy,sx) <- get
          ms <- get
          xs <- get
          let ls = M.fromList (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] xs)
          ys <- get
          let lm = M.fromList (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] ys)
          lme <- get
          return (Level nm hs sz ms ls lm lme)

type LMap = M.Map Loc (Tile.Tile, Tile.Tile)
type SMap = M.Map Loc Time

at, rememberAt :: LMap -> Loc -> Tile.Tile
at         l p = fst (M.findWithDefault (Tile.unknownTile, Tile.unknownTile) p l)
rememberAt l p = snd (M.findWithDefault (Tile.unknownTile, Tile.unknownTile) p l)

-- Checks for the presence of actors. Does *not* check if the tile is open.
unoccupied :: [Actor] -> Loc -> Bool
unoccupied actors loc =
  all (\ body -> aloc body /= loc) actors

-- Check whether one location is accessible from the other.
-- Precondition: the two locations are next to each other.
-- Currently only implements that the target location has to be open.
-- TODO: in the future check flying for chasms, swimming for water, etc.
accessible :: LMap -> Loc -> Loc -> Bool
accessible lm _source target =
  let tgt = lm `at` target
  in  Tile.isWalkable tgt

-- check whether the location contains a door of secrecy level lower than k
openable :: Int -> LMap -> Loc -> Bool
openable k lm target =
  let tgt = lm `at` target
  in Tile.hasFeature TileKind.Openable tgt ||
     (Tile.hasFeature TileKind.Hidden tgt &&
      fromJust (Tile.tsecret tgt) < k)

findLoc :: Level -> (Loc -> Tile.Tile -> Bool) -> Rnd Loc
findLoc l@(Level { lsize = sz, lmap = lm }) p =
  do
    loc <- locInArea ((0,0),sz)
    let tile = lm `at` loc
    if p loc tile
      then return loc
      else findLoc l p

findLocTry :: Int ->  -- try k times
              Level ->
              (Loc -> Tile.Tile -> Bool) ->  -- loop until satisfied
              (Loc -> Tile.Tile -> Bool) ->  -- only try to satisfy k times
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
dropItemsAt items loc lvl@(Level { lmap = lm }) =
  let joinItems = L.foldl' (\ acc i -> snd (joinItem i acc))
      t = lm `at` loc
      nt = t { Tile.titems = joinItems items (Tile.titems t) }
      ntRemember = lm `rememberAt` loc
  in  updateLMap (M.insert loc (nt, ntRemember)) lvl
