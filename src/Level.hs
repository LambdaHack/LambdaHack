module Level
  ( Party, SmellTime(..), SmellMap, SecretStrength(..), SecretMap
  , ItemMap, TileMap, Level(..)
  , updateHeroes, updateMonsters, updateLMap, updateLRMap, updateIMap
  , updateSmell , emptyParty, at, rememberAt, iat, irememberAt
  , accessible, openable, findLoc, findLocTry, dropItemsAt
  ) where

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
import qualified Tile
import qualified Feature as F
import qualified Kind

type Party = IM.IntMap Actor

newtype SmellTime = SmellTime{smelltime :: Time} deriving Show
instance Binary SmellTime where
  put = put . smelltime
  get = fmap SmellTime get
type SmellMap = IM.IntMap SmellTime

newtype SecretStrength = SecretStrength{secretStrength :: Time}
  deriving (Show, Eq, Ord)
instance Binary SecretStrength where
  put = put . secretStrength
  get = fmap SecretStrength get
type SecretMap = IM.IntMap SecretStrength

type ItemMap = IM.IntMap ([Item], [Item])

type TileMap = Kind.Array Loc TileKind

data Level = Level
  { lheroes   :: Party      -- ^ all heroes on the level
  , lxsize    :: X
  , lysize    :: Y
  , lmonsters :: Party      -- ^ all monsters on the level
  , lsmell    :: SmellMap
  , lsecret   :: SecretMap
  , litem     :: ItemMap
  , lmap      :: TileMap
  , lrmap     :: TileMap
  , lmeta     :: String
  , lstairs   :: (Loc, Loc) -- ^ here the stairs (down, up) from other levels end
  }
  deriving Show

updateLMap :: (TileMap -> TileMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }

updateLRMap :: (TileMap -> TileMap) -> Level -> Level
updateLRMap f lvl = lvl { lrmap = f (lrmap lvl) }

updateIMap :: (IM.IntMap ([Item], [Item]) ->
               IM.IntMap ([Item], [Item])) -> Level
              -> Level
updateIMap f lvl = lvl { litem = f (litem lvl) }

updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl { lsmell = f (lsmell lvl) }

updateMonsters :: (Party -> Party) -> Level -> Level
updateMonsters f lvl = lvl { lmonsters = f (lmonsters lvl) }

updateHeroes :: (Party -> Party) -> Level -> Level
updateHeroes f lvl = lvl { lheroes = f (lheroes lvl) }

emptyParty :: Party
emptyParty = IM.empty

instance Binary Level where
  put (Level hs sx sy ms ls le li lm lrm lme lstairs) =
        do
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
          return (Level hs sx sy ms ls le li lm lrm lme lstairs)

at, rememberAt :: Level -> Loc -> (Kind.Id TileKind)
at         l p = lmap l Kind.! p
rememberAt l p = lrmap l Kind.! p

-- Note: representations with 2 maps leads to longer code and slower 'remember'.
iat, irememberAt :: Level -> Loc -> [Item]
iat         l p = fst $ IM.findWithDefault ([], []) p (litem l)
irememberAt l p = snd $ IM.findWithDefault ([], []) p (litem l)

-- Check whether one location is accessible from the other.
-- Precondition: the two locations are next to each other.
-- Currently only implements that the target location has to be open.
-- TODO: in the future check flying for chasms, swimming for water, etc.
accessible :: Level -> Loc -> Loc -> Bool
accessible lvl _source target =
  let tgt = lvl `at` target
  in  Tile.isWalkable tgt

-- check whether the location contains a door of secrecy level lower than k
openable :: Level -> SecretStrength -> Loc -> Bool
openable lvl k target =
  let le = lsecret lvl
      tgt = lvl `at` target
  in Tile.hasFeature F.Openable tgt ||
     (Tile.hasFeature F.Hidden tgt &&
      le IM.! target < k)

-- Do not scatter items around, it's too much work for the player.
dropItemsAt :: [Item] -> Loc -> Level -> Level
dropItemsAt [] _loc = id
dropItemsAt items loc =
  let joinItems = L.foldl' (\ acc i -> snd (joinItem i acc))
      adj Nothing = Just (items, [])
      adj (Just (i, ri)) = Just (joinItems items i, ri)
  in  updateIMap (IM.alter adj loc)

findLoc :: TileMap -> (Loc -> (Kind.Id TileKind) -> Bool) -> Rnd Loc
findLoc lmap p =
  let (start, end) = Kind.bounds lmap
      search = do
        loc <- randomR (0, end)
        let tile = lmap Kind.! loc
        if p loc tile
          then return loc
          else search
  in assert (start == 0) $
     search

findLocTry ::
  Int ->  -- try to pTry only so many times
  TileMap ->
  (Loc -> Kind.Id TileKind -> Bool) ->  -- loop until satisfied
  (Loc -> Kind.Id TileKind -> Bool) ->  -- only try to satisfy k times
  Rnd Loc
findLocTry numTries lmap p pTry =
  let (start, end) = Kind.bounds lmap
      search k = do
        loc <- randomR (0, end)
        let tile = lmap Kind.! loc
        if p loc tile && pTry loc tile
          then return loc
          else if k > 1
            then search (k - 1)
            else findLoc lmap p
  in assert (numTries > 0 && start == 0) $
     search numTries
