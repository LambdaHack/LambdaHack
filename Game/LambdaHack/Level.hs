{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Level
  ( -- * Dungeon
    LevelId, Dungeon, initialLevel, ascendInBranch
    -- * The @Level@ type and its components
  , ActorDict, SmellMap, SecretMap, FloorMap, TileMap
  , Level(..)
    -- * Level update
  , updateActor, updateSmell, updateSecret, updateFloor, updateTile
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
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Random
import Game.LambdaHack.Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord,  Enum, Typeable)

instance Binary LevelId where
  put (LevelId n) = put n
  get = fmap LevelId get

-- | The complete dungeon is a map from level names to levels.
type Dungeon = EM.EnumMap LevelId Level

initialLevel :: LevelId
initialLevel = LevelId 1

-- | Levels in the current branch @k@ shallower than the current.
ascendInBranch :: Dungeon -> LevelId -> Int -> [LevelId]
ascendInBranch dungeon (LevelId n) k =
  let ln = LevelId $ n - k  -- currently just one branch
  in case EM.lookup ln dungeon of
    Nothing -> []
    Just _ -> [ln]

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point SmellTime

-- | Current secrecy value on map tiles.
type SecretMap = EM.EnumMap Point SecretTime

-- | Items located on map tiles.
type FloorMap = EM.EnumMap Point ItemBag

-- | Tile kinds on the map.
type TileMap = Kind.Array Point TileKind

-- | A view on single, inhabited dungeon level.
data Level = Level
  { ldepth  :: !Int             -- ^ depth of the level
  , lactor  :: !ActorDict       -- ^ remembered actors on the level
  , lfloor  :: !FloorMap        -- ^ remembered items lying on the floor
  , ltile   :: !TileMap         -- ^ remembered level map
  , lxsize  :: !X               -- ^ width of the level
  , lysize  :: !Y               -- ^ height of the level
  , lsmell  :: !SmellMap        -- ^ remembered smells on the level
  , ldesc   :: !Text            -- ^ level description
  , lstair  :: !(Point, Point)  -- ^ destination of (up, down) stairs
  , lseen   :: !Int             -- ^ number of clear tiles already seen
  , lclear  :: !Int             -- ^ total number of clear tiles
  , ltime   :: !Time            -- ^ date of the last activity on the level
  , lsecret :: !SecretMap       -- ^ remembered secrecy values
  }
  deriving Show

-- | Update the actor dictionary.
updateActor :: (ActorDict -> ActorDict) -> Level -> Level
updateActor f lvl = lvl {lactor = f (lactor lvl)}

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl {lsmell = f (lsmell lvl)}

-- | Update the secret map.
updateSecret :: (SecretMap -> SecretMap) -> Level -> Level
updateSecret f lvl = lvl {lsecret = f (lsecret lvl)}

-- | Update the items on the ground map.
updateFloor :: (FloorMap -> FloorMap) -> Level -> Level
updateFloor f lvl = lvl {lfloor = f (lfloor lvl)}

-- | Update the tile map.
updateTile :: (TileMap -> TileMap) -> Level -> Level
updateTile f lvl = lvl {ltile = f (ltile lvl)}

assertSparseItems :: FloorMap -> FloorMap
assertSparseItems m =
  assert (EM.null (EM.filter EM.null m) `blame` m) m

instance Binary Level where
  put Level{..} = do
    put ldepth
    put lactor
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
    put lsecret
  get = do
    ldepth <- get
    lactor <- get
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
    CFloor Point
  | CActor ActorId
  deriving (Show, Eq, Ord, Typeable)
