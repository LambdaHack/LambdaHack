{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Level
  ( -- * Dungeon
    LevelId, Dungeon, ascendInBranch
    -- * The @Level@ type and its components
  , SmellMap, SecretMap, ItemFloor, TileMap
  , Level(..)
    -- * Level update
  , updatePrio, updateSmell, updateSecret, updateFloor, updateTile
    -- * Level query
  , at, atI, accessible, openable, findPos, findPosTry
    -- * Item containers
  , Container(..)
 ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.List as L
import Data.Text (Text)
import Data.Typeable

import Game.LambdaHack.Actor
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Misc
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Random
import Game.LambdaHack.Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- | The complete dungeon is a map from level names to levels.
type Dungeon = EM.EnumMap LevelId Level

-- | Levels in the current branch @k@ shallower than the current.
ascendInBranch :: Dungeon -> LevelId -> Int -> [LevelId]
ascendInBranch dungeon lid k =
  let ln = toEnum $ fromEnum lid - k  -- currently just one branch
  in case EM.lookup ln dungeon of
    Nothing -> []
    Just _ -> [ln]

-- | Actor time priority queue.
type ActorPrio = EM.EnumMap Time [ActorId]

-- | Items located on map tiles.
type ItemFloor = EM.EnumMap Point ItemBag

-- | Tile kinds on the map.
type TileMap = Kind.Array Point TileKind

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point SmellTime

-- | Current secrecy value on map tiles.
type SecretMap = EM.EnumMap Point SecretTime

-- | A view on single, inhabited dungeon level. "Remembered" fields
-- carry a subset of the info in the client copies of levels.
data Level = Level
  { ldepth   :: !Int             -- ^ depth of the level
  , lprio    :: !ActorPrio       -- ^ remembered actor times on the level
  , lfloor   :: !ItemFloor       -- ^ remembered items lying on the floor
  , ltile    :: !TileMap         -- ^ remembered level map
  , lxsize   :: !X               -- ^ width of the level
  , lysize   :: !Y               -- ^ height of the level
  , lsmell   :: !SmellMap        -- ^ remembered smells on the level
  , ldesc    :: !Text            -- ^ level description
  , lstair   :: !(Point, Point)  -- ^ destination of (up, down) stairs
  , lseen    :: !Int             -- ^ currently remembered clear tiles
  , lclear   :: !Int             -- ^ total number of initially clear tiles
  , ltime    :: !Time            -- ^ date of the last activity on the level
  , litemNum :: !Int             -- ^ number of initial items, 0 for clients
  , lsecret  :: !SecretMap       -- ^ secrecy values; empty for clients
  }
  deriving (Show, Eq)

-- | Update the actor time priority queue.
updatePrio :: (ActorPrio -> ActorPrio) -> Level -> Level
updatePrio f lvl = lvl {lprio = f (lprio lvl)}

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl {lsmell = f (lsmell lvl)}

-- | Update the secret map.
updateSecret :: (SecretMap -> SecretMap) -> Level -> Level
updateSecret f lvl = lvl {lsecret = f (lsecret lvl)}

-- | Update the items on the ground map.
updateFloor :: (ItemFloor -> ItemFloor) -> Level -> Level
updateFloor f lvl = lvl {lfloor = f (lfloor lvl)}

-- | Update the tile map.
updateTile :: (TileMap -> TileMap) -> Level -> Level
updateTile f lvl = lvl {ltile = f (ltile lvl)}

assertSparseItems :: ItemFloor -> ItemFloor
assertSparseItems m =
  assert (EM.null (EM.filter EM.null m) `blame` m) m

instance Binary Level where
  put Level{..} = do
    put ldepth
    put lprio
    put (assertSparseItems lfloor)
    put ltile
    put lxsize
    put lysize
    put lsmell
    put ldesc
    put lstair
    put lseen
    put lclear
    put ltime
    put litemNum
    put lsecret
  get = do
    ldepth <- get
    lprio <- get
    lfloor <- get
    ltile <- get
    lxsize <- get
    lysize <- get
    lsmell <- get
    ldesc <- get
    lstair <- get
    lseen <- get
    lclear <- get
    ltime <- get
    litemNum <- get
    lsecret <- get
    return Level{..}

-- | Query for tile kinds on the map.
at :: Level -> Point -> Kind.Id TileKind
at Level{ltile}  p = ltile Kind.! p

-- | Query for items on the ground.
atI :: Level -> Point -> ItemBag
atI Level{lfloor} p = EM.findWithDefault EM.empty p lfloor

-- | Check whether one position is accessible from another,
-- using the formula from the standard ruleset.
accessible :: Kind.COps -> Level -> Point -> Point -> Bool
accessible Kind.COps{ cotile=Kind.Ops{okind=okind}, corule}
           lvl@Level{lxsize} spos tpos =
  let check = raccessible $ Kind.stdRuleset corule
      src = okind $ lvl `at` spos
      tgt = okind $ lvl `at` tpos
  in check lxsize spos src tpos tgt

-- | Check whether the position contains an openable tile.
openable :: Kind.Ops TileKind -> Level ->  Point -> Bool
openable cops lvl tpos =
  let tgt = lvl `at` tpos
  in hasFeature cops F.Openable tgt

-- | Find a random position on the map satisfying a predicate.
findPos :: TileMap -> (Point -> Kind.Id TileKind -> Bool) -> Rnd Point
findPos ltile p =
  let search = do
        pos <- randomR $ Kind.bounds ltile
        let tile = ltile Kind.! pos
        if p pos tile
          then return pos
          else search
  in search

-- | Try to find a random position on the map satisfying
-- the conjunction of the list of predicates.
-- If the permitted number of attempts is not enough,
-- try again the same number of times without the first predicate,
-- then without the first two, etc., until only one predicate remains,
-- at which point try as many times, as needed.
findPosTry :: Int                                  -- ^ the number of tries
           -> TileMap                              -- ^ look up in this map
           -> [Point -> Kind.Id TileKind -> Bool]  -- ^ predicates to satisfy
           -> Rnd Point
findPosTry _        ltile []        = findPos ltile (const (const True))
findPosTry _        ltile [p]       = findPos ltile p
findPosTry numTries ltile l@(_ : tl) = assert (numTries > 0) $
  let search 0 = findPosTry numTries ltile tl
      search k = do
        loc <- randomR $ Kind.bounds ltile
        let tile = ltile Kind.! loc
        if L.all (\ p -> p loc tile) l
          then return loc
          else search (k - 1)
  in search numTries

data Container =
    CFloor LevelId Point
  | CActor ActorId InvChar
  deriving (Show, Eq, Ord, Typeable)
