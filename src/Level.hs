module Level where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM

import Utils.Assert
import Geometry
import Loc
import Actor
import Item
import Content.TileKind
import Random
import WorldLoc
import qualified Tile
import qualified Feature as F
import qualified Kind

type Party = IM.IntMap Actor
type LAMap = Kind.Array Loc TileKind

newtype SmellTime = SmellTime{smelltime :: Time} deriving Show
instance Binary SmellTime where
  put = put . smelltime
  get = fmap SmellTime get
type SMap = IM.IntMap SmellTime

data Level = Level
  { lname     :: LevelId    -- TODO: remove
  , lheroes   :: Party      -- ^ all heroes on the level
  , lxsize    :: X
  , lysize    :: Y
  , lmonsters :: Party      -- ^ all monsters on the level
  , lsmell    :: SMap
  , lsecret   :: IM.IntMap Int
  , litem     :: IM.IntMap ([Item], [Item])
  , lmap      :: LAMap
  , lrmap     :: LAMap
  , lmeta     :: String
  , lstairs   :: (Loc, Loc) -- ^ here the stairs (down, up) from other levels end
  }
  deriving Show

updateLMap :: (LAMap -> LAMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }

updateLRMap :: (LAMap -> LAMap) -> Level -> Level
updateLRMap f lvl = lvl { lrmap = f (lrmap lvl) }

updateIMap :: (IM.IntMap ([Item], [Item]) ->
               IM.IntMap ([Item], [Item])) -> Level
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
  put (Level nm hs sx sy ms ls le li lm lrm lme lstairs) =
        do
          put nm
          put hs
          put sx
          put sy
          put ms
          put ls
          put le
          put (assert
                 (IM.null (IM.filter (\ (is1, is2) ->
                                       L.null is1 && L.null is2) li)
                 `blame` li) li)
          put lm
          put lrm
          put lme
          put lstairs
  get = do
          nm <- get
          hs <- get
          sx <- get
          sy <- get
          ms <- get
          ls <- get
          le <- get
          li <- get
          lm <- get
          lrm <- get
          lme <- get
          lstairs <- get
          return (Level nm hs sx sy ms ls le li lm lrm lme lstairs)

at, rememberAt :: Level -> Loc -> (Kind.Id TileKind)
at         l p = lmap l Kind.! p
rememberAt l p = lrmap l Kind.! p

-- Note: representations with 2 maps leads to longer code and slower 'remember'.
iat, irememberAt :: Level -> Loc -> [Item]
iat         l p = fst $ IM.findWithDefault ([], []) p (litem l)
irememberAt l p = snd $ IM.findWithDefault ([], []) p (litem l)

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
openable :: Int -> Level -> IM.IntMap Int -> Loc -> Bool
openable k lvl le target =
  let tgt = lvl `at` target
  in Tile.hasFeature F.Openable tgt ||
     (Tile.hasFeature F.Hidden tgt &&
      le IM.! target < k)

findLoc :: Level -> (Loc -> (Kind.Id TileKind) -> Bool) -> Rnd Loc
findLoc lvl@Level{lxsize, lysize} p =
  do
    loc <- randomR (0, lxsize * lysize - 1)
    let tile = lvl `at` loc
    if p loc tile
      then return loc
      else findLoc lvl p

findLocTry :: Int ->  -- try k times
              Level ->
              (Loc -> Kind.Id TileKind -> Bool) ->  -- loop until satisfied
              (Loc -> Kind.Id TileKind -> Bool) ->  -- only try to satisfy k times
              Rnd Loc
findLocTry k lvl@Level{lxsize, lysize} p pTry =
  do
    loc <- randomR (0, lxsize * lysize - 1)
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
  in  updateIMap (IM.alter adj loc)
