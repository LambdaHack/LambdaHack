{-# LANGUAGE DeriveGeneric #-}
-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Common.Level
  ( -- * Dungeon
    LevelId, Dungeon, ascendInBranch
    -- * Item containers
  , Container(..)
    -- * The @Level@ type and its components
  , SmellMap, ItemFloor, TileMap
  , Level(..)
    -- * Level update
  , updatePrio, updateSmell, updateFloor, updateTile
    -- * Level query
  , at, atI, accessible, accessibleDir, hideTile
  , findPos, findPosTry, mapLevelActors_, mapDungeonActors_
 ) where

import Data.Binary
import qualified Data.Bits as Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.List as L
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Tile
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Utils.Assert

-- | The complete dungeon is a map from level names to levels.
type Dungeon = EM.EnumMap LevelId Level

-- | Levels in the current branch, @k@ levels shallower than the current.
ascendInBranch :: Dungeon -> LevelId -> Int -> [LevelId]
ascendInBranch dungeon lid k =
  -- Currently there is just one branch, so the computation is simple.
  let depth = case EM.maxViewWithKey dungeon of
        Just ((d, _), _) -> fromEnum d
        Nothing -> assert `failure` (lid, k)
      ln = toEnum $ max 1 $ min depth $ fromEnum lid - k
  in case EM.lookup ln dungeon of
    Nothing -> []
    Just _ -> [ln]

-- | Item container type.
data Container =
    CFloor LevelId Point
  | CActor ActorId InvChar
  deriving (Show, Eq, Ord, Generic)

instance Binary Container

-- | Actor time priority queue.
type ActorPrio = EM.EnumMap Time [ActorId]

-- | Items located on map tiles.
type ItemFloor = EM.EnumMap Point ItemBag

-- | Tile kinds on the map.
type TileMap = Kind.Array Point TileKind

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point SmellTime

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
  , lsecret  :: !Int             -- ^ secret level seed, unknown by clients
  , lhidden  :: !Int             -- ^ secret tile density, unknown by clients
  }
  deriving (Show, Eq)

-- | Update the actor time priority queue.
updatePrio :: (ActorPrio -> ActorPrio) -> Level -> Level
updatePrio f lvl = lvl {lprio = f (lprio lvl)}

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl {lsmell = f (lsmell lvl)}

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
    put lhidden
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
    lhidden <- get
    return Level{..}

-- | Query for tile kinds on the map.
at :: Level -> Point -> Kind.Id TileKind
at Level{ltile} p = ltile Kind.! p

-- | Query for items on the ground.
atI :: Level -> Point -> ItemBag
atI Level{lfloor} p = EM.findWithDefault EM.empty p lfloor

-- | Check whether one position is accessible from another,
-- using the formula from the standard ruleset.
-- Precondition: the two positions are next to each other.
accessible :: Kind.COps -> Level -> Point -> Point -> Bool
accessible Kind.COps{cotile=Kind.Ops{okind=okind}, corule}
           lvl@Level{lxsize} spos tpos =
  assert (chessDist lxsize spos tpos == 1) $
  let check = raccessible $ Kind.stdRuleset corule
      src = okind $ lvl `at` spos
      tgt = okind $ lvl `at` tpos
  in check lxsize spos src tpos tgt

-- | Check whether actors can move from a position along a unit vector,
-- using the formula from the standard ruleset.
accessibleDir :: Kind.COps -> Level -> Point -> Vector -> Bool
accessibleDir cops lvl spos dir = accessible cops lvl spos $ spos `shift` dir

hideTile :: Kind.Ops TileKind -> Point -> Level -> Kind.Id TileKind
hideTile cotile p lvl =
  let t = lvl `at` p
      ht = Tile.hiddenAs cotile t  -- TODO; tabulate with Speedup?
  in if ht == t
        || (lsecret lvl `Bits.rotateR` fromEnum p `Bits.xor` fromEnum p)
           `mod` lhidden lvl == 0
     then ht
     else t

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
        pos <- randomR $ Kind.bounds ltile
        let tile = ltile Kind.! pos
        if L.all (\ p -> p pos tile) l
          then return pos
          else search (k - 1)
  in search numTries

mapLevelActors_ :: Monad m => (ActorId -> m a) -> Level -> m ()
mapLevelActors_ f Level{lprio} = do
  let as = concat $ EM.elems lprio
  mapM_ f as

mapDungeonActors_ :: Monad m => (ActorId -> m a) -> Dungeon -> m ()
mapDungeonActors_ f dungeon = do
  let ls = EM.elems dungeon
  mapM_ (mapLevelActors_ f) ls
