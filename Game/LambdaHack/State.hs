{-# LANGUAGE OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.State
  ( -- * Basic game state, local or global
    State
    -- * State components
  , sdungeon, sdepth, sdisco, sfaction, scops, sside, sarena
    -- * State operations
  , defStateGlobal, defStateLocal, switchGlobalSelectedSideOnlyForGlobalState
  , updateDungeon, updateDisco, updateFaction, updateCOps
  , updateArena, updateTime, updateSide, updateSelectedArena
  , getArena, getTime, getSide
  , isPlayerFaction, isSpawningFaction
    -- * Server state and its operations
  , StateServer(..), defStateServer
    -- * Client state and its operations
  , StateClient(..), defStateClient, defHistory
  , updateTarget, getTarget
  , invalidateSelectedLeader, updateSelectedLeader, getLeader
    -- * A dictionary of client connection information
  , ClientDict, ClientChan(..)
    -- * A dictionary of client states, for saving game.
  , StateDict
    -- * Components types and operations
  , TgtMode(..), Target(..)
  , Pers, FactionPers, dungeonPerception
  , DebugModeSer(..), cycleTryFov
  , DebugModeCli(..), toggleMarkVision, toggleMarkSmell, toggleOmniscient
  ) where

import Control.Concurrent.Chan
import Control.Monad
import Data.Binary
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Game.LambdaHack.Vector
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Actor
import Game.LambdaHack.Command
import Game.LambdaHack.Config
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- * Types

-- | View on game state. Clients never update @sdungeon@ and @sfaction@,
-- but the server updates it for them depending on client exploration.
-- Data invariant: no actor belongs to more than one @sdungeon@ level.
-- Each @sleader@ actor from any of the client states is on the @sarena@
-- level and belongs to @sside@ faction of the client's local state..
data State = State
  { _sdungeon :: !Dungeon      -- ^ remembered dungeon
  , _sdepth   :: !Int          -- ^ remembered dungeon depth
  , _sdisco   :: !Discoveries  -- ^ remembered item discoveries
  , _sfaction :: !FactionDict  -- ^ remembered sides still in game
  , _scops    :: Kind.COps     -- ^ remembered content
  , _sside    :: !FactionId    -- ^ faction of the selected actor
  , _sarena   :: !LevelId      -- ^ level of the selected actor
  }
  deriving Show

-- | Global, server state.
data StateServer = StateServer
  { sdiscoRev :: !DiscoRev      -- ^ reverse map, used for item creation
  , sflavour  :: !FlavourMap    -- ^ association of flavour to items
  , scounter  :: !Int           -- ^ stores next actor index
  , srandom   :: !R.StdGen      -- ^ current random generator
  , sconfig   :: !Config        -- ^ this game's config (including initial RNG)
  , squit     :: !(Maybe Bool)  -- ^ just going to save the game
  , sdebugSer :: !DebugModeSer  -- ^ debugging mode
  }
  deriving Show

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
-- Data invariant: if @_sleader@ is @Nothing@ then so is @srunning@.
data StateClient = StateClient
  { stgtMode  :: !(Maybe TgtMode)  -- ^ targeting mode
  , scursor   :: !Point    -- ^ cursor coordinates
  , seps      :: !Int      -- ^ a parameter of the tgt digital line
  , starget   :: !(IM.IntMap Target)  -- ^ targets of all actors in the dungeon
  , srunning  :: !(Maybe (Vector, Int))  -- ^ direction and distance of running
  , sreport   :: !Report        -- ^ current messages
  , shistory  :: !History       -- ^ history of messages
  , sper      :: !FactionPers   -- ^ faction perception indexed by levels
  , slastKey  :: !(Maybe K.KM)  -- ^ last command key pressed
  , _sleader  :: !(Maybe ActorId)  -- ^ selected actor
  , schan     :: ClientChan     -- ^ communication channels for this client
  , sdebugCli :: !DebugModeCli  -- ^ debugging mode
  }
  deriving Show

-- | Connection information for each client, indexed by faction identifier.
type ClientDict = IM.IntMap ClientChan

-- | Channels from client-server communication.
data ClientChan = ClientChan
  { toClient   :: Chan CmdCli
  , toServer :: Chan CmdSer
  }

instance Show ClientChan where
  show _ = "client channels"

-- | All client and local state, indexed by faction identifier.
type StateDict = IM.IntMap (StateClient, State)

-- | Current targeting mode of a client.
data TgtMode =
    TgtExplicit { tgtLevelId :: !LevelId }
            -- ^ the player requested targeting mode explicitly
  | TgtAuto     { tgtLevelId :: !LevelId }
            -- ^ the mode was entered (and will be exited) automatically
  deriving (Show, Eq)

-- | The type of na actor target.
data Target =
    TEnemy ActorId Point  -- ^ target an actor with its last seen position
  | TPos Point            -- ^ target a given position
  deriving (Show, Eq)

-- | Perception indexed by faction identifier.
type Pers = IM.IntMap FactionPers

-- | Perception of a single faction, indexed by level identifier.
type FactionPers = M.Map LevelId Perception

data DebugModeSer = DebugModeSer
  { stryFov :: !(Maybe FovMode) }
  deriving Show

data DebugModeCli = DebugModeCli
  { smarkVision :: !Bool
  , smarkSmell  :: !Bool
  , somniscient :: !Bool
  }
  deriving Show

-- * State operations

-- TODO: add a flag 'fresh' and when saving levels, don't save
-- and when loading regenerate this level.
unknownLevel :: Kind.Ops TileKind -> X -> Y
             -> Text -> (Point, Point) -> Int
             -> Level
unknownLevel Kind.Ops{ouniqGroup} lxsize lysize ldesc lstair lclear =
  let unknownId = ouniqGroup "unknown space"
  in Level { lactor = IM.empty
           , linv = IM.empty
           , litem = IM.empty
           , ltile = unknownTileMap unknownId lxsize lysize
           , lxsize = lxsize
           , lysize = lysize
           , lsmell = IM.empty
           , ldesc
           , lstair
           , lseen = 0
           , lclear
           , ltime = timeZero
           , lsecret = IM.empty
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
    { _sside = -1  -- no side yet selected
    , ..
    }

-- TODO: make lstair secret until discovered; use this later on for
-- goUp in targeting mode (land on stairs of on the same location up a level
-- if this set of stsirs is unknown).
-- | Initial per-faction local game state.
defStateLocal :: Dungeon
              -> Int -> Discoveries -> FactionDict
              -> Kind.COps -> LevelId -> FactionId
              -> State
defStateLocal globalDungeon
              _sdepth _sdisco _sfaction
              _scops@Kind.COps{cotile} _sarena _sside = do
  State
    { _sdungeon =
      M.map (\Level{lxsize, lysize, ldesc, lstair, lclear} ->
              unknownLevel cotile lxsize lysize ldesc lstair lclear)
            globalDungeon
    , ..
    }

-- | Switch selected faction within the global state. Local state side
-- is set at default level creation and never changes.
switchGlobalSelectedSideOnlyForGlobalState :: FactionId -> State -> State
switchGlobalSelectedSideOnlyForGlobalState fid s = s {_sside = fid}

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {_sdungeon = f (_sdungeon s)}

-- | Update item discoveries within state.
updateDisco :: (Discoveries -> Discoveries) -> State -> State
updateDisco f s = s { _sdisco = f (_sdisco s) }

-- | Update faction data within state.
updateFaction :: (FactionDict -> FactionDict) -> State -> State
updateFaction f s = s { _sfaction = f (_sfaction s) }

-- | Update content data within state.
updateCOps :: (Kind.COps -> Kind.COps) -> State -> State
updateCOps f s = s { _scops = f (_scops s) }

-- | Update current arena data within state.
updateArena :: (Level -> Level) -> State -> State
updateArena f s = updateDungeon (M.adjust f (_sarena s)) s

-- | Update time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = updateArena (\lvl@Level{ltime} -> lvl {ltime = f ltime}) s

-- | Update current side data within state.
updateSide :: (Faction -> Faction) -> State -> State
updateSide f s = updateFaction (IM.adjust f (_sside s)) s

-- | Update selected level within state.
updateSelectedArena :: LevelId -> State -> State
updateSelectedArena _sarena s = s {_sarena}

-- | Get current level from the dungeon data.
getArena :: State -> Level
getArena State{_sarena, _sdungeon} = _sdungeon M.! _sarena

-- | Get current time from the dungeon data.
getTime :: State -> Time
getTime State{_sarena, _sdungeon} = ltime $ _sdungeon M.! _sarena

-- | Get current faction from state.
getSide :: State -> Faction
getSide State{_sfaction, _sside} = _sfaction IM.! _sside

-- | Tell whether the faction is player-controlled.
isPlayerFaction :: State -> FactionId -> Bool
isPlayerFaction s fid = isNothing $ gAiSelected $ _sfaction s IM.! fid

-- | Tell whether the faction can spawn actors.
isSpawningFaction :: State -> FactionId -> Bool
isSpawningFaction s fid =
  let Kind.Ops{okind} = Kind.cofact (_scops s)
      kind = okind $ gkind $ _sfaction s IM.! fid
  in fspawn kind > 0

sdungeon :: State -> Dungeon
sdungeon = _sdungeon

sdepth :: State -> Int
sdepth = _sdepth

sdisco :: State -> Discoveries
sdisco = _sdisco

sfaction :: State -> FactionDict
sfaction = _sfaction

scops :: State -> Kind.COps
scops = _scops

sside :: State -> FactionId
sside = _sside

sarena :: State -> LevelId
sarena = _sarena

-- * StateServer operations

-- | Initial game server state.
defStateServer :: DiscoRev -> FlavourMap -> R.StdGen -> Config
                   -> StateServer
defStateServer sdiscoRev sflavour srandom sconfig =
  StateServer
    { scounter  = 0
    , squit     = Nothing
    , sdebugSer = defDebugModeSer
    , ..
    }

defDebugModeSer :: DebugModeSer
defDebugModeSer = DebugModeSer
  { stryFov = Nothing }

cycleTryFov :: StateServer -> StateServer
cycleTryFov s@StateServer{sdebugSer=sdebugSer@DebugModeSer{stryFov}} =
  s {sdebugSer = sdebugSer {stryFov = case stryFov of
                               Nothing          -> Just (Digital 100)
                               Just (Digital _) -> Just Permissive
                               Just Permissive  -> Just Shadow
                               Just Shadow      -> Just Blind
                               Just Blind       -> Nothing }}

-- * StateClient operations

-- | Initial game client state.
defStateClient :: Point -> ClientChan -> StateClient
defStateClient scursor schan = do
  StateClient
    { stgtMode  = Nothing
    , scursor
    , seps      = 0
    , starget   = IM.empty
    , srunning  = Nothing
    , sreport   = emptyReport
    , shistory  = emptyHistory
    , sper      = M.empty
    , _sleader  = Nothing  -- no heroes yet alive
    , slastKey  = Nothing
    , schan
    , sdebugCli = defDebugModeCli
    }

defDebugModeCli :: DebugModeCli
defDebugModeCli = DebugModeCli
  { smarkVision = False
  , smarkSmell  = False
  , somniscient = False
  }

defHistory :: IO History
defHistory = do
  dateTime <- getClockTime
  let curDate = MU.Text $ T.pack $ calendarTimeToString $ toUTCTime dateTime
  return $ singletonHistory $ singletonReport
         $ makeSentence ["Player history log started on", curDate]

-- | Update target parameters within client state.
updateTarget :: ActorId -> (Maybe Target -> Maybe Target) -> StateClient
             -> StateClient
updateTarget aid f cli = cli { starget = IM.alter f aid (starget cli) }

-- | Get target parameters from client state.
getTarget :: ActorId -> StateClient -> Maybe Target
getTarget aid cli = IM.lookup aid (starget cli)

-- | Invalidate selected actor, e.g., to avoid violatng the invariant.
-- If the the leader was running, stop the run.
invalidateSelectedLeader :: StateClient -> StateClient
invalidateSelectedLeader cli = cli {srunning = Nothing, _sleader = Nothing}

-- | Update selected actor within state. The actor is required
-- to belong to the selected level and selected faction.
updateSelectedLeader :: ActorId -> State -> StateClient -> StateClient
updateSelectedLeader leader s cli =
  let la = lactor $ _sdungeon s M.! _sarena s
      side1 = fmap bfaction $ IM.lookup leader la
      side2 = Just $ _sside s
  in assert (side1 == side2 `blame` (side1, side2, leader, _sarena s, s))
     $ cli {_sleader = Just leader}

getLeader :: StateClient -> Maybe ActorId
getLeader = _sleader

toggleMarkVision :: StateClient -> StateClient
toggleMarkVision s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkVision}} =
  s {sdebugCli = sdebugCli {smarkVision = not smarkVision}}

toggleMarkSmell :: StateClient -> StateClient
toggleMarkSmell s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkSmell}} =
  s {sdebugCli = sdebugCli {smarkSmell = not smarkSmell}}

toggleOmniscient :: StateClient -> StateClient
toggleOmniscient s@StateClient{sdebugCli=sdebugCli@DebugModeCli{somniscient}} =
  s {sdebugCli = sdebugCli {somniscient = not somniscient}}

-- | Calculate the perception of the whole dungeon.
dungeonPerception :: Kind.COps -> Config -> DebugModeSer -> State -> Pers
dungeonPerception cops sconfig sdebug s =
  let f fid _ = factionPerception cops sconfig sdebug s fid
  in IM.mapWithKey f $ sfaction s

-- | Calculate perception of a faction.
factionPerception :: Kind.COps -> Config -> DebugModeSer -> State -> FactionId
                  -> FactionPers
factionPerception cops sconfig sdebug s fid =
  M.map (levelPerception cops sconfig (stryFov sdebug) fid) $ sdungeon s

-- * Binary instances

instance Binary State where
  put State{..} = do
    put _sdungeon
    put _sdepth
    put _sdisco
    put _sfaction
    put _sside
    put _sarena
  get = do
    _sdungeon <- get
    _sdepth <- get
    _sdisco <- get
    _sfaction <- get
    _sside <- get
    _sarena <- get
    let _scops = undefined  -- overwritten by recreated cops
    return State{..}

instance Binary StateServer where
  put StateServer{..} = do
    put sdiscoRev
    put sflavour
    put scounter
    put (show srandom)
    put sconfig
  get = do
    sdiscoRev <- get
    sflavour <- get
    scounter <- get
    g <- get
    sconfig <- get
    let squit = Nothing
        srandom = read g
        sdebugSer = defDebugModeSer
    return StateServer{..}

instance Binary StateClient where
  put StateClient{..} = do
    put stgtMode
    put scursor
    put seps
    put starget
    put srunning
    put sreport
    put _sleader
  get = do
    stgtMode <- get
    scursor <- get
    seps <- get
    starget <- get
    srunning <- get
    sreport <- get
    _sleader <- get
    let shistory = emptyHistory
        sper = M.empty
        slastKey = Nothing
        schan = undefined  -- overwritten by recreated channels
        sdebugCli = defDebugModeCli
    return StateClient{..}

instance Binary TgtMode where
  put (TgtExplicit l) = putWord8 0 >> put l
  put (TgtAuto     l) = putWord8 1 >> put l
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM TgtExplicit get
      1 -> liftM TgtAuto get
      _ -> fail "no parse (TgtMode)"

instance Binary Target where
  put (TEnemy a ll) = putWord8 0 >> put a >> put ll
  put (TPos pos) = putWord8 1 >> put pos
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 TEnemy get get
      1 -> liftM TPos get
      _ -> fail "no parse (Target)"
