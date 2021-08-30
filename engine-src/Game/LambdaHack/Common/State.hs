-- | The common, for server and clients, main game state type
-- and its operations.
module Game.LambdaHack.Common.State
  ( -- * Basic game state, local or global
    State
    -- * State components
  , sdungeon, stotalDepth, sactorD, sitemD, sitemIxMap, sfactionD, stime, scops
  , sgold, shigh, sgameModeId, sdiscoKind, sdiscoAspect, sactorMaxSkills
    -- * State construction
  , defStateGlobal, emptyState, localFromGlobal
    -- * State update
  , updateDungeon, updateDepth, updateActorD, updateItemD, updateItemIxMap
  , updateFactionD, updateTime, updateCOpsAndCachedData, updateGold
  , updateDiscoKind, updateDiscoAspect, updateActorMaxSkills
    -- * State operations
  , getItemBody, aspectRecordFromItem, aspectRecordFromIid
  , maxSkillsFromActor, maxSkillsInDungeon
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , unknownLevel, unknownTileMap
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.CaveKind (CaveKind)
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- | The main game state, the basic one, pertaining to a single game,
-- not to a single playing session or an intersection of both.
-- This state persists between playing sessions, until the particular game ends.
-- Anything that persists between games is stored in server state,
-- client state or client UI session state.
--
-- Another differentiating property of this state is that it's kept
-- separately on the server and each of the clients (players, human or AI)
-- and separately updated, according to what each player can observe.
-- It's never updated directly, but always through atomic commands
-- ("CmdAtomic") that are filtered and interpreted differently
-- on server and on each client. Therefore, the type is a view on the
-- game state, not the real game state, except on the server that
-- alone stores the full game information.
data State = State
  { _sdungeon        :: Dungeon    -- ^ remembered dungeon
  , _stotalDepth     :: Dice.AbsDepth
                                   -- ^ absolute dungeon depth for item creation
  , _sactorD         :: ActorDict  -- ^ remembered actors in the dungeon
  , _sitemD          :: ItemDict   -- ^ remembered items in the dungeon
  , _sitemIxMap      :: ItemIxMap  -- ^ spotted items with the same kind index
                                   --   could be recomputed at resume, but small
  , _sfactionD       :: FactionDict
                                   -- ^ remembered sides still in game
  , _stime           :: Time       -- ^ global game time, for UI display only
  , _scops           :: COps       -- ^ remembered content
  , _sgold           :: Int        -- ^ total value of human trinkets in dungeon
  , _shigh           :: HighScore.ScoreDict  -- ^ high score table
  , _sgameModeId     :: ContentId ModeKind   -- ^ current game mode
  , _sdiscoKind      :: DiscoveryKind        -- ^ item kind discoveries data
  , _sdiscoAspect    :: DiscoveryAspect
                                   -- ^ item aspect data; could be recomputed
  , _sactorMaxSkills :: ActorMaxSkills
                                   -- ^ actor maximal skills; is recomputed
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
    put _sgold
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
    _sgold <- get
    _shigh <- get
    _sgameModeId <- get
    _sdiscoKind <- get
    _sdiscoAspect <- get
    let _scops = emptyCOps
        _sactorMaxSkills = EM.empty
    return $! State{..}

sdungeon :: State -> Dungeon
sdungeon = _sdungeon

stotalDepth :: State -> Dice.AbsDepth
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

sgold :: State -> Int
sgold = _sgold

shigh :: State -> HighScore.ScoreDict
shigh = _shigh

sgameModeId :: State -> ContentId ModeKind
sgameModeId = _sgameModeId

sdiscoKind :: State -> DiscoveryKind
sdiscoKind = _sdiscoKind

sdiscoAspect :: State -> DiscoveryAspect
sdiscoAspect = _sdiscoAspect

sactorMaxSkills :: State -> ActorMaxSkills
sactorMaxSkills = _sactorMaxSkills

unknownLevel :: COps -> ContentId CaveKind -> Dice.AbsDepth -> Area
             -> ([Point], [Point]) -> [Point] -> Int -> Bool
             -> Level
unknownLevel COps{corule, cotile}
             lkind ldepth larea lstair lescape lexpl lnight =
  let outerId = ouniqGroup cotile TK.S_UNKNOWN_OUTER_FENCE
  in Level { lkind
           , ldepth
           , lfloor = EM.empty
           , lembed = EM.empty
           , lbig = EM.empty
           , lproj = EM.empty
           , ltile = unknownTileMap larea outerId (rXmax corule) (rYmax corule)
           , lentry = EM.empty
           , larea
           , lsmell = EM.empty
           , lstair
           , lescape
           , lseen = 0
           , lexpl
           , ltime = timeZero
           , lnight
           }

unknownTileMap :: Area -> ContentId TileKind -> X -> Y -> TileMap
unknownTileMap larea outerId rXmax rYmax =
  let unknownMap = PointArray.replicateA rXmax rYmax TK.unknownId
      outerUpdate = zip (areaInnerBorder larea) $ repeat outerId
  in unknownMap PointArray.// outerUpdate

-- | Initial complete global game state.
defStateGlobal :: Dungeon -> Dice.AbsDepth -> FactionDict -> COps
               -> HighScore.ScoreDict -> ContentId ModeKind -> DiscoveryKind
               -> State
defStateGlobal _sdungeon _stotalDepth _sfactionD _scops _shigh _sgameModeId
               _sdiscoKind =
  State
    { _sactorD = EM.empty
    , _sitemD = EM.empty
    , _sitemIxMap = EM.empty
    , _stime = timeZero
    , _sgold = 0
    , _sdiscoAspect = EM.empty
    , _sactorMaxSkills = EM.empty
    , ..
    }

-- | Initial empty state.
emptyState :: State
emptyState =
  State
    { _sdungeon = EM.empty
    , _stotalDepth = Dice.AbsDepth 0
    , _sactorD = EM.empty
    , _sitemD = EM.empty
    , _sitemIxMap = EM.empty
    , _sfactionD = EM.empty
    , _stime = timeZero
    , _scops = emptyCOps
    , _sgold = 0
    , _shigh = HighScore.empty
    , _sgameModeId = toEnum 0  -- the initial value is unused
    , _sdiscoKind = EM.empty
    , _sdiscoAspect = EM.empty
    , _sactorMaxSkills = EM.empty
    }

-- | Local state created by removing secret information from global
-- state components.
localFromGlobal :: State -> State
localFromGlobal State{..} =
  State
    { _sdungeon =
      EM.map (\Level{..} ->
              unknownLevel _scops lkind ldepth larea
                           lstair lescape lexpl lnight)
             _sdungeon
    , ..
    }

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {_sdungeon = f (_sdungeon s)}

-- | Update dungeon depth.
updateDepth :: (Dice.AbsDepth -> Dice.AbsDepth) -> State -> State
updateDepth f s = s {_stotalDepth = f (_stotalDepth s)}

-- | Update the actor dictionary.
updateActorD :: (ActorDict -> ActorDict) -> State -> State
{-# INLINE updateActorD #-}  -- just in case inliner goes hiwire
updateActorD f s = s {_sactorD = f (_sactorD s)}

-- | Update the item dictionary.
updateItemD :: (ItemDict -> ItemDict) -> State -> State
{-# INLINE updateItemD #-}
updateItemD f s = s {_sitemD = f (_sitemD s)}

-- | Update the item kind index map.
updateItemIxMap :: (ItemIxMap -> ItemIxMap) -> State -> State
updateItemIxMap f s = s {_sitemIxMap = f (_sitemIxMap s)}

-- | Update faction data within state.
updateFactionD :: (FactionDict -> FactionDict) -> State -> State
updateFactionD f s = s {_sfactionD = f (_sfactionD s)}

-- | Update global time within state.
updateTime :: (Time -> Time) -> State -> State
{-# INLINE updateTime #-}
updateTime f s = s {_stime = f (_stime s)}

-- | Update content data within state and recompute the cached data.
updateCOpsAndCachedData :: (COps -> COps) -> State -> State
updateCOpsAndCachedData f s =
  let s2 = s {_scops = f (_scops s)}
  in s2 {_sactorMaxSkills = maxSkillsInDungeon s2}

-- | Update total gold value in the dungeon.
updateGold :: (Int -> Int) -> State -> State
updateGold f s = s {_sgold = f (_sgold s)}

updateDiscoKind :: (DiscoveryKind -> DiscoveryKind) -> State -> State
updateDiscoKind f s = s {_sdiscoKind = f (_sdiscoKind s)}

updateDiscoAspect :: (DiscoveryAspect -> DiscoveryAspect) -> State -> State
updateDiscoAspect f s = s {_sdiscoAspect = f (_sdiscoAspect s)}

updateActorMaxSkills :: (ActorMaxSkills -> ActorMaxSkills) -> State -> State
updateActorMaxSkills f s = s {_sactorMaxSkills = f (_sactorMaxSkills s)}

getItemBody :: ItemId -> State -> Item
getItemBody iid s = sitemD s EM.! iid

-- This is best guess, including mean aspect record, so we can take into
-- consideration even the kind the item hides under.
aspectRecordFromItem :: ItemId -> Item -> State -> IA.AspectRecord
aspectRecordFromItem iid item s =
  let kindId = case jkind item of
        IdentityObvious ik -> ik
        IdentityCovered ix ik -> fromMaybe ik $ ix `EM.lookup` sdiscoKind s
      COps{coItemSpeedup} = scops s
      mean = IA.kmMean $ getKindMean kindId coItemSpeedup
  in fromMaybe mean $ EM.lookup iid $ sdiscoAspect s

aspectRecordFromIid :: ItemId -> State -> IA.AspectRecord
aspectRecordFromIid iid s = aspectRecordFromItem iid (getItemBody iid s) s

maxSkillsFromActor :: Actor -> State -> Ability.Skills
maxSkillsFromActor b s =
  let processIid (iid, (k, _)) = (IA.aSkills $ aspectRecordFromIid iid s, k)
      processBag sks = Ability.sumScaledSkills $ map processIid sks
  in processBag $ EM.assocs (borgan b) ++ EM.assocs (beqp b)

maxSkillsInDungeon :: State -> ActorMaxSkills
maxSkillsInDungeon s =
  EM.map (`maxSkillsFromActor` s) $ sactorD s
