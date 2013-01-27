-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Level
  ( -- * The @Level@ type and its components
    ActorDict, InvDict, SmellMap, SecretMap, ItemMap, TileMap
  , Level(..)
    -- * Level update
  , updateActor, updateInv, updateSmell, updateIMap, updateLMap, dropItemsAt
    -- * Level query
  , at, atI, accessible, openable, findPos, findPosTry
    -- * Dungeon
  , LevelId, levelNumber, levelDefault, Dungeon
  ) where

import Data.Binary
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Actor
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Random
import Game.LambdaHack.Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

-- | Items carried by actors, indexed by actor identifier.
type InvDict = EM.EnumMap ActorId [Item]

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point SmellTime

-- | Current secrecy value on map tiles.
type SecretMap = EM.EnumMap Point SecretTime

-- | Item lists on map tiles.
type ItemMap = EM.EnumMap Point [Item]

-- | Tile kinds on the map.
type TileMap = Kind.Array Point TileKind

-- | A view on single, inhabited dungeon level.
data Level = Level
  { lactor  :: !ActorDict       -- ^ remembered actors on the level
  , linv    :: !InvDict         -- ^ remembered actor inventories
  , litem   :: !ItemMap         -- ^ remembered items on the level
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

-- | Update the actor map.
updateActor :: (ActorDict -> ActorDict) -> Level -> Level
updateActor f lvl = lvl { lactor = f (lactor lvl) }

-- | Update the actor items map.
updateInv :: (InvDict -> InvDict) -> Level -> Level
updateInv f lvl = lvl { linv = f (linv lvl) }

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl { lsmell = f (lsmell lvl) }

-- | Update the items on the ground map.
updateIMap :: (ItemMap -> ItemMap) -> Level -> Level
updateIMap f lvl = lvl { litem = f (litem lvl) }

-- | Update the tile map.
updateLMap :: (TileMap -> TileMap) -> Level -> Level
updateLMap f lvl = lvl { ltile = f (ltile lvl) }

-- Note: do not scatter items around, it's too much work for the player.
-- | Place all items on the list at a position on the level.
dropItemsAt :: [Item] -> Point -> Level -> Level
dropItemsAt [] _pos = id
dropItemsAt items loc =
  let joinItems = L.foldl' (\ acc i -> snd (joinItem i acc))
      adj Nothing = Just items
      adj (Just i) = Just $ joinItems items i
  in  updateIMap (EM.alter adj loc)

assertSparseItems :: ItemMap -> ItemMap
assertSparseItems m =
  assert (EM.null (EM.filter (\ is -> L.null is) m) `blame` m) m

instance Binary Level where
  put Level{..} = do
    put lactor
    put linv
    put (assertSparseItems litem)
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
    lactor <- get
    linv <- get
    litem <- get
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
atI :: Level -> Point -> [Item]
atI Level{litem} p = EM.findWithDefault [] p litem

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

-- | Depth of a level.
levelNumber :: LevelId -> Int
levelNumber (LambdaCave n) = n

-- | Default level for a given depth.
levelDefault :: Int -> LevelId
levelDefault = LambdaCave

-- | The complete dungeon is a map from level names to levels.
type Dungeon = M.Map LevelId Level
