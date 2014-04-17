-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Common.Level
  ( -- * Dungeon
    LevelId, Dungeon, ascendInBranch
    -- * The @Level@ type and its components
  , Level(..), ActorPrio, ItemFloor, TileMap, SmellMap
    -- * Level query
  , at, atI, checkAccess, checkDoorAccess, accessible, accessibleDir
  , isSecretPos, hideTile
  , findPos, findPosTry, mapLevelActors_, mapDungeonActors_
 ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.Bits as Bits
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Tile
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind

-- | The complete dungeon is a map from level names to levels.
type Dungeon = EM.EnumMap LevelId Level

-- | Levels in the current branch, @k@ levels shallower than the current.
ascendInBranch :: Dungeon -> Int -> LevelId -> [LevelId]
ascendInBranch dungeon k lid =
  -- Currently there is just one branch, so the computation is simple.
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "null dungeon" `twith` dungeon
      ln = max minD $ min maxD $ toEnum $ fromEnum lid + k
  in case EM.lookup ln dungeon of
    Just _ | ln /= lid -> [ln]
    _ -> []

-- | Actor time priority queue.
type ActorPrio = EM.EnumMap Time [ActorId]

-- | Items located on map tiles.
type ItemFloor = EM.EnumMap Point ItemBag

-- | Tile kinds on the map.
type TileMap = PointArray.Array (Kind.Id TileKind)

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point SmellTime

-- | A view on single, inhabited dungeon level. "Remembered" fields
-- carry a subset of the info in the client copies of levels.
data Level = Level
  { ldepth    :: !Int        -- ^ depth of the level
  , lprio     :: !ActorPrio  -- ^ remembered actor times on the level
  , lfloor    :: !ItemFloor  -- ^ remembered items lying on the floor
  , ltile     :: !TileMap    -- ^ remembered level map
  , lxsize    :: !X          -- ^ width of the level
  , lysize    :: !Y          -- ^ height of the level
  , lsmell    :: !SmellMap   -- ^ remembered smells on the level
  , ldesc     :: !Text       -- ^ level description
  , lstair    :: !([Point], [Point])
                             -- ^ destinations of (up, down) stairs
  , lseen     :: !Int        -- ^ currently remembered clear tiles
  , lclear    :: !Int        -- ^ total number of initially clear tiles
  , ltime     :: !Time       -- ^ date of the last activity on the level
  , litemNum  :: !Int        -- ^ number of initial items, 0 for clients
  , litemFreq :: !(Frequency Text)  -- ^ frequency of initial items,
                                    --   [] for clients
  , lsecret   :: !Int        -- ^ secret tile seed
  , lhidden   :: !Int        -- ^ secret tile density
  , lescape   :: !Bool       -- ^ has an Effect.Escape tile
  }
  deriving (Show, Eq)

assertSparseItems :: ItemFloor -> ItemFloor
assertSparseItems m =
  assert (EM.null (EM.filter EM.null m)
          `blame` "null floors found" `twith` m) m

-- | Query for tile kinds on the map.
at :: Level -> Point -> Kind.Id TileKind
at Level{ltile} p = ltile PointArray.! p

-- | Query for items on the ground.
atI :: Level -> Point -> ItemBag
atI Level{lfloor} p = EM.findWithDefault EM.empty p lfloor

checkAccess :: Kind.COps -> Level -> Maybe (Point -> Point -> Bool)
checkAccess Kind.COps{corule} _ =
  case raccessible $ Kind.stdRuleset corule of
    Nothing -> Nothing
    Just ch -> Just $ \spos tpos -> ch spos tpos

checkDoorAccess :: Kind.COps -> Level -> Maybe (Point -> Point -> Bool)
checkDoorAccess Kind.COps{corule, cotile} lvl =
  case raccessibleDoor $ Kind.stdRuleset corule of
    Nothing -> Nothing
    Just chDoor ->
      Just $ \spos tpos ->
        let st = lvl `at` spos
            tt = lvl `at` tpos
        in not (Tile.isDoor cotile st || Tile.isDoor cotile tt)
           || chDoor spos tpos

-- | Check whether one position is accessible from another,
-- using the formula from the standard ruleset.
-- Precondition: the two positions are next to each other.
accessible :: Kind.COps -> Level -> Point -> Point -> Bool
accessible cops@Kind.COps{cotile} lvl =
  let checkWalkability =
        Just $ \_ tpos -> Tile.isWalkable cotile $ lvl `at` tpos
      conditions = catMaybes [ checkWalkability
                             , checkAccess cops lvl
                             , checkDoorAccess cops lvl ]
  in \spos tpos -> all (\f -> f spos tpos) conditions

-- | Check whether actors can move from a position along a unit vector,
-- using the formula from the standard ruleset.
accessibleDir :: Kind.COps -> Level -> Point -> Vector -> Bool
accessibleDir cops lvl spos dir = accessible cops lvl spos $ spos `shift` dir

isSecretPos :: Level -> Point -> Bool
isSecretPos lvl (Point x y) =
  (lsecret lvl `Bits.rotateR` x `Bits.xor` y + x) `mod` lhidden lvl == 0

hideTile :: Kind.Ops TileKind -> Level -> Point -> Kind.Id TileKind
hideTile cotile lvl p =
  let t = lvl `at` p
      ht = Tile.hideAs cotile t  -- TODO; tabulate with Speedup?
  in if isSecretPos lvl p then ht else t

-- | Find a random position on the map satisfying a predicate.
findPos :: TileMap -> (Point -> Kind.Id TileKind -> Bool) -> Rnd Point
findPos ltile p =
  let (x, y) = PointArray.sizeA ltile
      search = do
        px <- randomR (0, x - 1)
        py <- randomR (0, y - 1)
        let pos = Point{..}
            tile = ltile PointArray.! pos
        if p pos tile
          then return $! pos
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
           -> (Point -> Kind.Id TileKind -> Bool)  -- ^ mandatory predicate
           -> [Point -> Kind.Id TileKind -> Bool]  -- ^ optional predicates
           -> Rnd Point
findPosTry _        ltile m []         = findPos ltile m
findPosTry numTries ltile m l@(_ : tl) = assert (numTries > 0) $
  let (x, y) = PointArray.sizeA ltile
      search 0 = findPosTry numTries ltile m tl
      search k = do
        px <- randomR (0, x - 1)
        py <- randomR (0, y - 1)
        let pos = Point{..}
            tile = ltile PointArray.! pos
        if m pos tile && all (\p -> p pos tile) l
          then return $! pos
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
    put litemFreq
    put lsecret
    put lhidden
    put lescape
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
    litemFreq <- get
    lsecret <- get
    lhidden <- get
    lescape <- get
    return $! Level{..}
