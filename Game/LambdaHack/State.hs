{-# LANGUAGE OverloadedStrings #-}
-- | Game state and persistent player cli types and operations.
module Game.LambdaHack.State
  ( State
  , sdungeon, sdepth, sdisco, sfaction, scops, splayer, sside, sarena
  , defStateGlobal, defStateLocal
  , updateDungeon, updateArena, updateTime, updateDiscoveries, updateSide
  , getArena, getTime
  , isControlledFaction, isSpawningFaction
  , StateServer(..), defStateServer
  , StateClient(..), defStateClient, defHistory, updateTarget
  , StateDict
  , TgtMode(..), Target(..)
  , DebugModeSer(..), cycleTryFov
  , DebugModeCli(..), toggleMarkVision, toggleMarkSmell, toggleOmniscient
  ) where

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
import Game.LambdaHack.Config
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Time

-- * Types

-- TODO: check the invariant each time splayer, sarena and sside is modified.
-- More specifically: if splayer is on sarena, check he belongs to sside,
-- and if not, which is rare, check he's not on any other level.
-- TODO: perhaps we'd be safer if splayer was set to invalidActor
-- whenever he's not in the dungeon?
-- TDDO: perhpas update some of the 3 fields together to keep the invariant?
-- | View on game state. Clients never update @sdungeon@ and @sfaction@,
-- but the server updates it for them depending on client exploration.
-- Data invariant: no actor belongs to more than one sdungeon level.
-- Actor splayer is not on any other sdungeon level than sarena.
-- If splayer is on sarena, he belongs to sside.
data State = State
  { sdungeon :: !Dungeon      -- ^ remembered dungeon
  , sdepth   :: !Int          -- ^ remembered dungeon depth
  , sdisco   :: !Discoveries  -- ^ remembered item discoveries
  , sfaction :: !FactionDict  -- ^ remembered sides still in game
  , scops    :: Kind.COps     -- ^ remembered content
  , splayer  :: !ActorId      -- ^ selected actor
  , sside    :: !FactionId    -- ^ faction of the selected actor
  , sarena   :: !LevelId      -- ^ level of the selected actor
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
data StateClient = StateClient
  { stgtMode  :: !TgtMode  -- ^ targeting mode
  , scursor   :: !Point    -- ^ cursor coordinates
  , seps      :: !Int      -- ^ a parameter of the tgt digital line
  , starget   :: !(IM.IntMap Target)  -- ^ targets of all actors in the dungeon
  , srunning  :: !(Maybe (Vector, Int))  -- ^ direction and distance of running
  , sreport   :: !Report        -- ^ current messages
  , shistory  :: !History       -- ^ history of messages
  , slastKey  :: !(Maybe K.KM)  -- ^ last command key pressed
  , sdebugCli :: !DebugModeCli  -- ^ debugging mode
  }

-- | All client and local state, indexed by faction identifier.
type StateDict = IM.IntMap (StateClient, State)

-- | All factions in the game, indexed by faction identifier.
type FactionDict = IM.IntMap Faction

-- | Current targeting mode of the player.
data TgtMode =
    TgtOff  -- ^ not in targeting mode
  | TgtExplicit { tgtLevelId :: !LevelId }
            -- ^ the player requested targeting mode explicitly
  | TgtAuto     { tgtLevelId :: !LevelId }
            -- ^ the mode was entered (and will be exited) automatically
  deriving (Show, Eq)

-- | The type of na actor target.
data Target =
    TEnemy ActorId Point  -- ^ target an actor with its last seen position
  | TPos Point            -- ^ target a given position
  deriving (Show, Eq)

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
               -> FactionDict -> Kind.COps -> FactionId -> LevelId
               -> State
defStateGlobal sdungeon sdepth sdisco sfaction scops sside sarena =
  State
    { splayer = invalidActorId  -- no heroes yet alive
    , ..
    }

-- TODO: make lstair secret until discovered; use this later on for
-- goUp in targeting mode (land on stairs of on the same location up a level
-- if this set of stsirs is unknown).
-- | Initial per-faction local game state.
defStateLocal :: Dungeon
              -> Int -> Discoveries -> FactionDict
              -> Kind.COps -> FactionId -> LevelId
              -> State
defStateLocal globalDungeon
              sdepth sdisco sfaction
              scops@Kind.COps{cotile} sside sarena = do
  State
    { splayer  = invalidActorId  -- no heroes yet alive
    , sdungeon =
      M.map (\Level{lxsize, lysize, ldesc, lstair, lclear} ->
              unknownLevel cotile lxsize lysize ldesc lstair lclear)
            globalDungeon
    , ..
    }

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {sdungeon = f (sdungeon s)}

-- | Update current arena data within state.
updateArena :: (Level -> Level) -> State -> State
updateArena f s = updateDungeon (M.adjust f (sarena s)) s

-- | Update time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = updateArena (\lvl@Level{ltime} -> lvl {ltime = f ltime}) s

-- | Update item discoveries within state.
updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdisco = f (sdisco s) }

-- | Update current side data within state.
updateSide :: (Faction -> Faction) -> State -> State
updateSide f s = s {sfaction = IM.adjust f (sside s) (sfaction s)}

-- | Get current level from the dungeon data.
getArena :: State -> Level
getArena State{sarena, sdungeon} = sdungeon M.! sarena

-- | Get current time from the dungeon data.
getTime :: State -> Time
getTime State{sarena, sdungeon} = ltime $ sdungeon M.! sarena

-- | Tell whether the faction is human-controlled.
isControlledFaction :: State -> FactionId -> Bool
isControlledFaction s fid = isNothing $ gAiSelected $ sfaction s IM.! fid

-- | Tell whether the faction is human-controlled.
isSpawningFaction :: State -> FactionId -> Bool
isSpawningFaction s fid =
  let Kind.Ops{okind} = Kind.cofact (scops s)
      kind = okind $ gkind $ sfaction s IM.! fid
  in fspawn kind > 0

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
defStateClient :: Point -> StateClient
defStateClient ppos = do
  StateClient
    { stgtMode = TgtOff
    , scursor = ppos
    , seps      = 0
    , starget   = IM.empty
    , srunning  = Nothing
    , sreport   = emptyReport
    , shistory  = emptyHistory
    , slastKey  = Nothing
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

-- | Update target parameters within state.
updateTarget :: ActorId -> (Maybe Target -> Maybe Target) -> StateClient
             -> StateClient
updateTarget actor f s = s { starget = IM.alter f actor (starget s) }

toggleMarkVision :: StateClient -> StateClient
toggleMarkVision s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkVision}} =
  s {sdebugCli = sdebugCli {smarkVision = not smarkVision}}

toggleMarkSmell :: StateClient -> StateClient
toggleMarkSmell s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkSmell}} =
  s {sdebugCli = sdebugCli {smarkSmell = not smarkSmell}}

toggleOmniscient :: StateClient -> StateClient
toggleOmniscient s@StateClient{sdebugCli=sdebugCli@DebugModeCli{somniscient}} =
  s {sdebugCli = sdebugCli {somniscient = not somniscient}}

-- * Binary instances

instance Binary State where
  put State{..} = do
    put sdungeon
    put sdepth
    put sdisco
    put sfaction
    put splayer
    put sside
    put sarena
  get = do
    sdungeon <- get
    sdepth <- get
    sdisco <- get
    sfaction <- get
    splayer <- get
    sside <- get
    sarena <- get
    let scops = undefined  -- overwritten by recreated cops
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
    put shistory
  get = do
    stgtMode <- get
    scursor  <- get
    seps       <- get
    starget <- get
    srunning <- get
    sreport <- get
    shistory <- get
    let slastKey = Nothing
        sdebugCli = defDebugModeCli
    return StateClient{..}

instance Binary TgtMode where
  put TgtOff      = putWord8 0
  put (TgtExplicit l) = putWord8 1 >> put l
  put (TgtAuto     l) = putWord8 2 >> put l
  get = do
    tag <- getWord8
    case tag of
      0 -> return TgtOff
      1 -> liftM TgtExplicit get
      2 -> liftM TgtAuto get
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
