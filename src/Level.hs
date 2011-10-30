module Level where

import Data.Binary
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.IntMap as IM

import Utils.Assert
import Geometry
import GeometryRnd
import Actor
import Item
import Random
import WorldLoc
import qualified Tile
import qualified Feature as F

type Party = IM.IntMap Actor
type LMap = M.Map Loc (Tile.Tile, Tile.Tile)

newtype SmellTime = SmellTime{smelltime :: Time} deriving Show
instance Binary SmellTime where
  put = put . smelltime
  get = fmap SmellTime get
type SMap = M.Map Loc SmellTime

data Level = Level
  { lname     :: LevelId    -- TODO: remove
  , lheroes   :: Party      -- ^ all heroes on the level
  , lsize     :: (Y,X)  -- TODO: change to size (is lower right point)
  , lmonsters :: Party      -- ^ all monsters on the level
  , lsmell    :: SMap
  , lsecret   :: M.Map Loc Int
  , litem     :: M.Map Loc ([Item], [Item])
  , lmap      :: LMap
  , lmeta     :: String
  , lstairs   :: (Loc, Loc) -- ^ here the stairs (down, up) from other levels end
  }
  deriving Show

updateLMap :: (LMap -> LMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }

updateIMap :: (M.Map Loc ([Item], [Item]) -> M.Map Loc ([Item], [Item])) -> Level
              -> Level
updateIMap f lvl = lvl { litem = f (litem lvl) }

updateSMap :: (SMap -> SMap) -> Level -> Level
updateSMap f lvl = lvl { lsmell = f (lsmell lvl) }

updateMonsters :: (Party -> Party) -> Level -> Level
updateMonsters f lvl = lvl { lmonsters = f (lmonsters lvl) }

updateHeroes :: (Party -> Party) -> Level -> Level
updateHeroes f lvl = lvl { lheroes = f (lheroes lvl) }

emptyParty :: Party
emptyParty = IM.empty

instance Binary Level where
  put (Level nm hs sz@(sy,sx) ms ls le li lm lme lstairs) =
        do
          put nm
          put hs
          put sz
          put ms
          put ls
          put le
          put (assert
                 (M.null (M.filter (\ (is1, is2) -> L.null is1 && L.null is2) li)
                 `blame` li) li)
          put ((sy+1)*(sx+1)) >> mapM_ put (M.elems lm)
          put lme
          put lstairs
  get = do
          nm <- get
          hs <- get
          sz@(sy,sx) <- get
          ms <- get
          ls <- get
          le <- get
          li <- get
          ys <- get
          let lm = M.fromDistinctAscList
                     (zip [ (y,x) | y <- [0..sy], x <- [0..sx] ] ys)
          lme <- get
          lstairs <- get
          return (Level nm hs sz ms ls le li lm lme lstairs)

at, rememberAt :: Level -> Loc -> Tile.Tile
at         l p = fst $ (lmap l) M.! p
rememberAt l p = snd $ (lmap l) M.! p

-- Note: representations with 2 maps leads to longer code and slower 'remember'.
iat, irememberAt :: Level -> Loc -> [Item]
iat         l p = fst $ M.findWithDefault ([], []) p (litem l)
irememberAt l p = snd $ M.findWithDefault ([], []) p (litem l)

-- Checks for the presence of actors. Does *not* check if the tile is open.
unoccupied :: [Actor] -> Loc -> Bool
unoccupied actors loc =
  all (\ body -> aloc body /= loc) actors

-- Check whether one location is accessible from the other.
-- Precondition: the two locations are next to each other.
-- Currently only implements that the target location has to be open.
-- TODO: in the future check flying for chasms, swimming for water, etc.
accessible :: Level -> Loc -> Loc -> Bool
accessible lvl _source target =
  let tgt = lvl `at` target
  in  Tile.isWalkable tgt

-- check whether the location contains a door of secrecy level lower than k
openable :: Int -> Level -> M.Map Loc Int -> Loc -> Bool
openable k lvl le target =
  let tgt = lvl `at` target
  in Tile.hasFeature F.Openable tgt ||
     (Tile.hasFeature F.Hidden tgt &&
      le M.! target < k)

findLoc :: Level -> (Loc -> Tile.Tile -> Bool) -> Rnd Loc
findLoc lvl@Level{lsize} p =
  do
    loc <- locInArea ((0,0), lsize)
    let tile = lvl `at` loc
    if p loc tile
      then return loc
      else findLoc lvl p

findLocTry :: Int ->  -- try k times
              Level ->
              (Loc -> Tile.Tile -> Bool) ->  -- loop until satisfied
              (Loc -> Tile.Tile -> Bool) ->  -- only try to satisfy k times
              Rnd Loc
findLocTry k lvl@Level{lsize} p pTry =
  do
    loc <- locInArea ((0,0), lsize)
    let tile = lvl `at` loc
    if p loc tile && pTry loc tile
      then return loc
      else if k > 1
             then findLocTry (k - 1) lvl p pTry
             else findLoc lvl p

-- Do not scatter items around, it's too much work for the player.
dropItemsAt :: [Item] -> Loc -> Level -> Level
dropItemsAt [] _loc = id
dropItemsAt items loc =
  let joinItems = L.foldl' (\ acc i -> snd (joinItem i acc))
      adj Nothing = Just (items, [])
      adj (Just (i, ri)) = Just (joinItems items i, ri)
  in  updateIMap (M.alter adj loc)
