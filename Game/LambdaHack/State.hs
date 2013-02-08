{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.State
  ( -- * Basic game state, local or global
    State
    -- * State components
  , sdungeon, sdepth, sitem, sdisco, sfaction, scops, squit
  , sside, sarena
    -- * State operations
  , defStateGlobal, defStateLocal, localFromGlobal
  , switchGlobalSelectedSideOnlyForGlobalState
  , updateDungeon, updateItem, updateDisco, updateFaction
  , updateCOps, updateQuit
  , updateArena, updateTime, updateSide, updateSelectedArena
  , getArena, getTime, getSide
  , isHumanFaction, isSpawningFaction
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)
import Data.Typeable

import Game.LambdaHack.Actor
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Time

-- | View on game state. "Remembered" fields carry a subset of the info
-- in the client copies of the state.
--
-- Data invariant: no actor belongs to more than one @sdungeon@ level.
-- Each @sleader@ actor from any of the client states is on the @sarena@
-- level and belongs to @sside@ faction of the client's local state.
-- Note: we use @_sdepth@ instead of computing maximal depth whenever needed,
-- to keep the dungeon (which can be huge) lazy.
data State = State
  { _sdungeon :: !Dungeon      -- ^ remembered dungeon
  , _sdepth   :: !Int          -- ^ remembered dungeon depth
  , _sitem    :: !ItemDict     -- ^ remembered items in the dungeon
  , _sdisco   :: !Discoveries  -- ^ remembered item discoveries
  , _sfaction :: !FactionDict  -- ^ remembered sides still in game
  , _scops    :: Kind.COps     -- ^ remembered content
  , _squit    :: !(Maybe Bool)  -- ^ just about to save the game
  , _sside    :: !FactionId    -- ^ faction of the selected actor
  , _sarena   :: !LevelId      -- ^ level of the selected actor
  }
  deriving (Show, Typeable)

-- TODO: add a flag 'fresh' and when saving levels, don't save
-- and when loading regenerate this level.
unknownLevel :: Kind.Ops TileKind -> Int -> X -> Y
             -> Text -> (Point, Point) -> Int
             -> Level
unknownLevel Kind.Ops{ouniqGroup} ldepth lxsize lysize ldesc lstair lclear =
  let unknownId = ouniqGroup "unknown space"
  in Level { ldepth
           , lactor = EM.empty
           , lfloor = EM.empty
           , ltile = unknownTileMap unknownId lxsize lysize
           , lxsize = lxsize
           , lysize = lysize
           , lsmell = EM.empty
           , ldesc
           , lstair
           , lseen = 0
           , lclear
           , ltime = timeZero
           , lsecret = EM.empty
           }

unknownTileMap :: Kind.Id TileKind -> Int -> Int -> TileMap
unknownTileMap unknownId cxsize cysize =
  let bounds = (origin, toPoint cxsize $ PointXY (cxsize - 1, cysize - 1))
  in Kind.listArray bounds (repeat unknownId)

-- | Initial complete global game state.
defStateGlobal :: Dungeon -> Int -> Discoveries
               -> FactionDict -> Kind.COps -> LevelId
               -> State
defStateGlobal _sdungeon _sdepth _sdisco _sfaction _scops _sarena =
  State
    { _sside = invalidFactionId  -- no side yet selected
    , _squit = Nothing
    , _sitem = EM.empty
    , ..
    }

-- | Initial per-faction local game state.
defStateLocal :: Kind.COps -> FactionId -> State
defStateLocal _scops _sside =
  State
    { _sdungeon = EM.empty
    , _sdepth = 0
    , _sitem = EM.empty
    , _sdisco = EM.empty
    , _sfaction = EM.empty
    , _scops
    , _squit = Nothing
    , _sside
    , _sarena = initialLevel
    }

-- TODO: make lstair secret until discovered; use this later on for
-- goUp in targeting mode (land on stairs of on the same location up a level
-- if this set of stsirs is unknown).
-- | Local state created by removing secret information from global
-- state components.
localFromGlobal :: State -> State
localFromGlobal State{ _scops=_scops@Kind.COps{ coitem=Kind.Ops{okind}
                                              , corule
                                              , cotile }
                      , .. } =
  State
    { _sdungeon =
      EM.map (\Level{ldepth, lxsize, lysize, ldesc, lstair, lclear} ->
              unknownLevel cotile ldepth lxsize lysize ldesc lstair lclear)
            _sdungeon
    , _sdisco = let f ik = isymbol (okind ik)
                           `notElem` (ritemProject $ Kind.stdRuleset corule)
                in EM.filter f _sdisco
    , _sside = invalidFactionId  -- will be set by the client
    , _squit = Nothing
    , ..
    }

-- | Switch selected faction within the global state. Local state side
-- is set at default level creation and never changes.
switchGlobalSelectedSideOnlyForGlobalState :: FactionId -> State -> State
switchGlobalSelectedSideOnlyForGlobalState fid s = s {_sside = fid}

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {_sdungeon = f (_sdungeon s)}

-- | Update the items dictionary.
updateItem :: (ItemDict -> ItemDict) -> State -> State
updateItem f s = s {_sitem = f (_sitem s)}

-- | Update item discoveries within state.
updateDisco :: (Discoveries -> Discoveries) -> State -> State
updateDisco f s = s {_sdisco = f (_sdisco s)}

-- | Update faction data within state.
updateFaction :: (FactionDict -> FactionDict) -> State -> State
updateFaction f s = s {_sfaction = f (_sfaction s)}

-- | Update content data within state.
updateCOps :: (Kind.COps -> Kind.COps) -> State -> State
updateCOps f s = s {_scops = f (_scops s)}

-- | Update game save status.
updateQuit :: (Maybe Bool -> Maybe Bool) -> State -> State
updateQuit f s = s {_squit = f (_squit s)}

-- | Update current arena data within state.
updateArena :: (Level -> Level) -> State -> State
updateArena f s = updateDungeon (EM.adjust f (_sarena s)) s

-- | Update time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = updateArena (\lvl@Level{ltime} -> lvl {ltime = f ltime}) s

-- | Update current side data within state.
updateSide :: (Faction -> Faction) -> State -> State
updateSide f s = updateFaction (EM.adjust f (_sside s)) s

-- | Update selected level within state.
updateSelectedArena :: LevelId -> State -> State
updateSelectedArena _sarena s = s {_sarena}

-- | Get current level from the dungeon data.
getArena :: State -> Level
getArena State{_sarena, _sdungeon} = _sdungeon EM.! _sarena

-- | Get current time from the dungeon data.
getTime :: State -> Time
getTime State{_sarena, _sdungeon} = ltime $ _sdungeon EM.! _sarena

-- | Get current faction from state.
getSide :: State -> Faction
getSide State{_sfaction, _sside} = _sfaction EM.! _sside

-- | Tell whether the faction is human player-controlled.
isHumanFaction :: State -> FactionId -> Bool
isHumanFaction s fid = isHumanFact $ _sfaction s EM.! fid

-- | Tell whether the faction can spawn actors.
isSpawningFaction :: State -> FactionId -> Bool
isSpawningFaction s fid = isSpawningFact (_scops s) $ _sfaction s EM.! fid

sdungeon :: State -> Dungeon
sdungeon = _sdungeon

sdepth :: State -> Int
sdepth = _sdepth

sitem :: State -> ItemDict
sitem = _sitem

sdisco :: State -> Discoveries
sdisco = _sdisco

sfaction :: State -> FactionDict
sfaction = _sfaction

scops :: State -> Kind.COps
scops = _scops

squit :: State -> Maybe Bool
squit = _squit

sside :: State -> FactionId
sside = _sside

sarena :: State -> LevelId
sarena = _sarena

instance Binary State where
  put State{..} = do
    put _sdungeon
    put _sdepth
    put _sitem
    put _sdisco
    put _sfaction
    put _squit
    put _sside
    put _sarena
  get = do
    _sdungeon <- get
    _sdepth <- get
    _sitem <- get
    _sdisco <- get
    _sfaction <- get
    _squit <- get
    _sside <- get
    _sarena <- get
    let _scops = undefined  -- overwritten by recreated cops
    return State{..}
