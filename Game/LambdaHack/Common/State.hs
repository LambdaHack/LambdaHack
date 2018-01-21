-- | The common server and client basic game state type and its operations.
module Game.LambdaHack.Common.State
  ( -- * Basic game state, local or global
    State
    -- * State components
  , sdungeon, stotalDepth, sactorD, sitemD, sitemIxMap, sfactionD, stime, scops
  , shigh, sgameModeId, sdiscoKind, sdiscoAspect, sactorAspect
    -- * State construction
  , defStateGlobal, emptyState, localFromGlobal
    -- * State update
  , updateDungeon, updateDepth, updateActorD, updateItemD, updateItemIxMap
  , updateFactionD, updateTime, updateCOps
  , updateDiscoKind, updateDiscoAspect, updateActorAspect
    -- * State operations
  , getItemBody, aspectRecordFromItem, aspectRecordFromIid
  , aspectRecordFromActor, actorAspectInDungeon
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , unknownLevel, unknownTileMap
#endif
 ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.CaveKind (CaveKind)
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.TileKind (TileKind, unknownId)

-- | View on the basic game state.
-- The @remembered@ fields, in client copies of the state, carry only
-- a subset of the full information that the server keeps.
-- Clients never directly change their @State@, but apply
-- atomic actions sent by the server to do so (and/or the server applies
-- the actions to each client state in turn).
data State = State
  { _sdungeon     :: Dungeon      -- ^ remembered dungeon
  , _stotalDepth  :: AbsDepth     -- ^ absolute dungeon depth, for item creation
  , _sactorD      :: ActorDict    -- ^ remembered actors in the dungeon
  , _sitemD       :: ItemDict     -- ^ remembered items in the dungeon
  , _sitemIxMap   :: ItemIxMap    -- ^ spotted items with the same kind index
  , _sfactionD    :: FactionDict  -- ^ remembered sides still in game
  , _stime        :: Time         -- ^ global game time, for UI display only
  , _scops        :: COps         -- ^ remembered content
  , _shigh        :: HighScore.ScoreDict  -- ^ high score table
  , _sgameModeId  :: ContentId ModeKind   -- ^ current game mode
  , _sdiscoKind   :: DiscoveryKind     -- ^ item kind discoveries data
  , _sdiscoAspect :: DiscoveryAspect   -- ^ item aspect data
  , _sactorAspect :: ActorAspect       -- ^ actor aspect data
  }
  deriving (Show, Eq)

instance Binary State where
  put State{..} = do
    put _sdungeon
    put _stotalDepth
    put _sactorD
    put _sitemD
    put _sitemIxMap
    put _sfactionD
    put _stime
    put _shigh
    put _sgameModeId
    put _sdiscoKind
    put _sdiscoAspect
  get = do
    _sdungeon <- get
    _stotalDepth <- get
    _sactorD <- get
    _sitemD <- get
    _sitemIxMap <- get
    _sfactionD <- get
    _stime <- get
    _shigh <- get
    _sgameModeId <- get
    _sdiscoKind <- get
    _sdiscoAspect <- get
    let _scops = emptyCOps
        sNoActorAspect = State{_sactorAspect = EM.empty, ..}
        _sactorAspect = actorAspectInDungeon sNoActorAspect
    return $! State{..}

sdungeon :: State -> Dungeon
sdungeon = _sdungeon

stotalDepth :: State -> AbsDepth
stotalDepth = _stotalDepth

sactorD :: State -> ActorDict
sactorD = _sactorD

sitemD :: State -> ItemDict
sitemD = _sitemD

sitemIxMap :: State -> ItemIxMap
sitemIxMap = _sitemIxMap

sfactionD :: State -> FactionDict
sfactionD = _sfactionD

stime :: State -> Time
stime = _stime

scops :: State -> COps
scops = _scops

shigh :: State -> HighScore.ScoreDict
shigh = _shigh

sgameModeId :: State -> ContentId ModeKind
sgameModeId = _sgameModeId

sdiscoKind :: State -> DiscoveryKind
sdiscoKind = _sdiscoKind

sdiscoAspect :: State -> DiscoveryAspect
sdiscoAspect = _sdiscoAspect

sactorAspect :: State -> ActorAspect
sactorAspect = _sactorAspect

unknownLevel :: COps -> ContentId CaveKind -> AbsDepth -> X -> Y
             -> ([Point], [Point]) -> [Point] -> Int -> Bool
             -> Level
unknownLevel COps{cotile}
             lkind ldepth lxsize lysize lstair lescape lexplorable lnight =
  let outerId = ouniqGroup cotile "basic outer fence"
  in Level { lkind
           , ldepth
           , lfloor = EM.empty
           , lembed = EM.empty
           , lactor = EM.empty
           , ltile = unknownTileMap outerId lxsize lysize
           , lxsize
           , lysize
           , lsmell = EM.empty
           , lstair
           , lescape
           , lseen = 0
           , lexplorable
           , ltime = timeZero
           , lnight
           }

unknownTileMap :: ContentId TileKind -> Int -> Int -> TileMap
unknownTileMap outerId lxsize lysize =
  let unknownMap = PointArray.replicateA lxsize lysize unknownId
      borders = [ Point x y
                | x <- [0, lxsize - 1], y <- [1..lysize - 2] ]
                ++ [ Point x y
                   | x <- [0..lxsize - 1], y <- [0, lysize - 1] ]
      outerUpdate = zip borders $ repeat outerId
  in unknownMap PointArray.// outerUpdate

-- | Initial complete global game state.
defStateGlobal :: Dungeon -> AbsDepth -> FactionDict -> COps
               -> HighScore.ScoreDict -> ContentId ModeKind -> DiscoveryKind
               -> State
defStateGlobal _sdungeon _stotalDepth _sfactionD _scops _shigh _sgameModeId
               _sdiscoKind =
  State
    { _sactorD = EM.empty
    , _sitemD = EM.empty
    , _sitemIxMap = EM.empty
    , _stime = timeZero
    , _sdiscoAspect = EM.empty
    , _sactorAspect = EM.empty
    , ..
    }

-- | Initial empty state.
emptyState :: State
emptyState =
  State
    { _sdungeon = EM.empty
    , _stotalDepth = AbsDepth 0
    , _sactorD = EM.empty
    , _sitemD = EM.empty
    , _sitemIxMap = EM.empty
    , _sfactionD = EM.empty
    , _stime = timeZero
    , _scops = emptyCOps
    , _shigh = HighScore.empty
    , _sgameModeId = minBound  -- the initial value is unused
    , _sdiscoKind = EM.empty
    , _sdiscoAspect = EM.empty
    , _sactorAspect = EM.empty
    }

-- | Local state created by removing secret information from global
-- state components.
localFromGlobal :: State -> State
localFromGlobal State{..} =
  State
    { _sdungeon =
      EM.map (\Level{..} ->
              unknownLevel _scops lkind ldepth lxsize lysize
                           lstair lescape lexplorable lnight)
             _sdungeon
    , ..
    }

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {_sdungeon = f (_sdungeon s)}

-- | Update dungeon depth.
updateDepth :: (AbsDepth -> AbsDepth) -> State -> State
updateDepth f s = s {_stotalDepth = f (_stotalDepth s)}

-- | Update the actor dictionary.
updateActorD :: (ActorDict -> ActorDict) -> State -> State
updateActorD f s = s {_sactorD = f (_sactorD s)}

-- | Update the item dictionary.
updateItemD :: (ItemDict -> ItemDict) -> State -> State
updateItemD f s = s {_sitemD = f (_sitemD s)}

-- | Update the item kind index map.
updateItemIxMap :: (ItemIxMap -> ItemIxMap) -> State -> State
updateItemIxMap f s = s {_sitemIxMap = f (_sitemIxMap s)}

-- | Update faction data within state.
updateFactionD :: (FactionDict -> FactionDict) -> State -> State
updateFactionD f s = s {_sfactionD = f (_sfactionD s)}

-- | Update global time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = s {_stime = f (_stime s)}

-- | Update content data within state.
updateCOps :: (COps -> COps) -> State -> State
updateCOps f s = s {_scops = f (_scops s)}

updateDiscoKind :: (DiscoveryKind -> DiscoveryKind) -> State -> State
updateDiscoKind f s = s {_sdiscoKind = f (_sdiscoKind s)}

updateDiscoAspect :: (DiscoveryAspect -> DiscoveryAspect) -> State -> State
updateDiscoAspect f s = s {_sdiscoAspect = f (_sdiscoAspect s)}

updateActorAspect :: (ActorAspect -> ActorAspect) -> State -> State
updateActorAspect f s = s {_sactorAspect = f (_sactorAspect s)}

getItemBody :: ItemId -> State -> Item
getItemBody iid s = sitemD s EM.! iid

aspectRecordFromItem :: ItemId -> Item -> State -> AspectRecord
aspectRecordFromItem iid item s =
  case EM.lookup iid (sdiscoAspect s) of
    Just ar -> ar
    Nothing -> case EM.lookup (jkindIx item) (sdiscoKind s) of
        Just KindMean{kmMean} -> kmMean
        Nothing -> emptyAspectRecord

aspectRecordFromIid :: ItemId -> State -> AspectRecord
aspectRecordFromIid iid s = aspectRecordFromItem iid (getItemBody iid s) s

aspectRecordFromActor :: Actor -> State -> AspectRecord
aspectRecordFromActor b s =
  let processIid (iid, (k, _)) = (aspectRecordFromIid iid s, k)
      processBag ass = sumAspectRecord $ map processIid ass
  in processBag $ EM.assocs (borgan b) ++ EM.assocs (beqp b)

actorAspectInDungeon :: State -> ActorAspect
actorAspectInDungeon s =
  EM.map (`aspectRecordFromActor` s) $ sactorD s
