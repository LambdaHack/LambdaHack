-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Level
  ( -- * The @Level@ type and its components
    ActorDict, InvDict, SmellMap, SecretMap, ItemMap, TileMap, Level(..)
    -- * Level update
  , updateActorDict, updateInv
  , updateSmell, updateIMap, updateIRMap, updateLMap, updateLRMap, dropItemsAt
    -- * Level query
  , at, rememberAt, atI, rememberAtI
  , accessible, openable, findLoc, findLocTry
  ) where

import Data.Binary
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Text (Text)

import Game.LambdaHack.Actor
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
type ActorDict = IM.IntMap Actor

-- | Items carried by actors, indexed by actor identifier.
type InvDict = IM.IntMap [Item]

-- | Current smell on map tiles.
type SmellMap = IM.IntMap SmellTime

-- | Current secrecy value on map tiles.
type SecretMap = IM.IntMap SecretTime

-- | Item lists on map tiles.
type ItemMap = IM.IntMap [Item]

-- | Tile kinds on the map.
type TileMap = Kind.Array Point TileKind

-- | A single, inhabited dungeon level.
data Level = Level
  { lactor  :: !ActorDict       -- ^ all actors on the level
  , linv    :: !InvDict         -- ^ items belonging to actors
  , lxsize  :: !X               -- ^ width of the level
  , lysize  :: !Y               -- ^ height of the level
  , lsmell  :: !SmellMap        -- ^ smells
  , lsecret :: !SecretMap       -- ^ secrecy values
  , litem   :: !ItemMap         -- ^ items on the ground
  , lritem  :: !ItemMap         -- ^ remembered items on the ground
  , lmap    :: !TileMap         -- ^ map tiles
  , lrmap   :: !TileMap         -- ^ remembered map tiles
  , ldesc   :: !Text            -- ^ level description for the player
  , lmeta   :: !Text            -- ^ debug information from cave generation
  , lstairs :: !(Point, Point)  -- ^ destination of the (up, down) stairs
  , ltime   :: !Time            -- ^ date of the last activity on the level
  , lclear  :: !Int             -- ^ total number of clear tiles
  , lseen   :: !Int             -- ^ number of clear tiles already seen
  }
  deriving Show

-- | Update the hero and monster maps.
updateActorDict :: (ActorDict -> ActorDict) -> Level -> Level
updateActorDict f lvl = lvl { lactor = f (lactor lvl) }

-- | Update the hero items and monster items maps.
updateInv :: (InvDict -> InvDict) -> Level -> Level
updateInv f lvl = lvl { linv = f (linv lvl) }

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl { lsmell = f (lsmell lvl) }

-- | Update the items on the ground map.
updateIMap, updateIRMap:: (ItemMap -> ItemMap) -> Level -> Level
updateIMap f lvl = lvl { litem = f (litem lvl) }
updateIRMap f lvl = lvl { lritem = f (lritem lvl) }

-- | Update the tile and remembered tile maps.
updateLMap, updateLRMap :: (TileMap -> TileMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }
updateLRMap f lvl = lvl { lrmap = f (lrmap lvl) }

-- Note: do not scatter items around, it's too much work for the player.
-- | Place all items on the list at a location on the level.
dropItemsAt :: [Item] -> Point -> Level -> Level
dropItemsAt [] _loc = id
dropItemsAt items loc =
  let joinItems = L.foldl' (\ acc i -> snd (joinItem i acc))
      adj Nothing = Just items
      adj (Just i) = Just $ joinItems items i
  in  updateIMap (IM.alter adj loc)

assertSparseItems :: ItemMap -> ItemMap
assertSparseItems m =
  assert (IM.null (IM.filter (\ is -> L.null is) m) `blame` m) m

instance Binary Level where
  put Level{..} = do
    put lactor
    put linv
    put lxsize
    put lysize
    put lsmell
    put lsecret
    put (assertSparseItems litem)
    put (assertSparseItems lritem)
    put lmap
    put lrmap
    put ldesc
    put lmeta
    put lstairs
    put ltime
    put lclear
    put lseen
  get = do
    lactor <- get
    linv <- get
    lxsize <- get
    lysize <- get
    lsmell <- get
    lsecret <- get
    litem <- get
    lritem <- get
    lmap <- get
    lrmap <- get
    ldesc <- get
    lmeta <- get
    lstairs <- get
    ltime <- get
    lclear <- get
    lseen <- get
    return Level{..}

-- | Query for actual and remembered tile kinds on the map.
at, rememberAt :: Level -> Point -> Kind.Id TileKind
at         Level{lmap}  p = lmap Kind.! p
rememberAt Level{lrmap} p = lrmap Kind.! p

-- Note: representations with 2 maps leads to longer code and slower 'remember'.
-- | Query for actual and remembered items on the ground.
atI, rememberAtI :: Level -> Point -> [Item]
atI         Level{litem}  p = IM.findWithDefault [] p litem
rememberAtI Level{lritem} p = IM.findWithDefault [] p lritem

-- | Check whether one location is accessible from another,
-- using the formula from the standard ruleset.
accessible :: Kind.COps -> Level -> Point -> Point -> Bool
accessible Kind.COps{ cotile=Kind.Ops{okind=okind}, corule}
           lvl@Level{lxsize} sloc tloc =
  let check = raccessible $ Kind.stdRuleset corule
      src = okind $ lvl `at` sloc
      tgt = okind $ lvl `at` tloc
  in check lxsize sloc src tloc tgt

-- | Check whether the location contains a door of secrecy lower than @k@
-- and that can be opened according to the standard ruleset.
openable :: Kind.Ops TileKind -> Level -> SecretTime -> Point -> Bool
openable cops lvl@Level{lsecret} k target =
  let tgt = lvl `at` target
  in hasFeature cops F.Openable tgt ||
     (hasFeature cops F.Hidden tgt &&
      lsecret IM.! target <= k)

-- | Find a random location on the map satisfying a predicate.
findLoc :: TileMap -> (Point -> Kind.Id TileKind -> Bool) -> Rnd Point
findLoc lmap p =
  let search = do
        loc <- randomR $ Kind.bounds lmap
        let tile = lmap Kind.! loc
        if p loc tile
          then return loc
          else search
  in search

-- | Try to find a random location on the map satisfying
-- the conjunction of the list of predicates.
-- If the premitted number of attempts is not enough,
-- try again the same number of times without the first predicate,
-- then without the first two, etc., until only one predicate remains,
-- at which point try as many times, as needed.
findLocTry :: Int                                  -- ^ the number of tries
           -> TileMap                              -- ^ look up in this map
           -> [Point -> Kind.Id TileKind -> Bool]  -- ^ predicates to satisfy
           -> Rnd Point
findLocTry _        lmap []        = findLoc lmap (const (const True))
findLocTry _        lmap [p]       = findLoc lmap p
findLocTry numTries lmap l@(_ : tl) = assert (numTries > 0) $
  let search 0 = findLocTry numTries lmap tl
      search k = do
        loc <- randomR $ Kind.bounds lmap
        let tile = lmap Kind.! loc
        if L.all (\ p -> p loc tile) l
          then return loc
          else search (k - 1)
  in search numTries
