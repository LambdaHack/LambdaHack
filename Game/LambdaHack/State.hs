{-# LANGUAGE OverloadedStrings #-}
-- | Game state and persistent player cli types and operations.
module Game.LambdaHack.State
  ( -- * Game state
    TgtMode(..), Cursor(..), Status(..)
  , State(..), defStateGlobal, defStateLocal
  , StateServer(..), defStateServer
  , StateClient(..), defStateClient, defHistory
  , StateDict
    -- * Type of na actor target
  , Target(..), updateTarget
    -- * Accessor
  , getArena, getTime, isControlledFaction, isSpawningFaction
    -- * State update
  , updateCursor, updateTime, updateDiscoveries, updateArena, updateDungeon
    -- * Textual description
  , lookAt
    -- * Debug flags
  , DebugModeSer(..), defDebugModeSer, cycleTryFov
  , DebugModeCli(..), defDebugModeCli, toggleMarkVision, toggleMarkSmell
  , toggleOmniscient
  ) where

import Control.Monad
import Data.Binary
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time
import Game.LambdaHack.Vector

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

-- | View on game state. Clients never update @sdungeon@ and @sfaction@,
-- but the server updates it for them depending on client exploration.
data State = State
  { sdungeon :: !Dungeon      -- ^ remembered dungeon
  , sdepth   :: !Int          -- ^ remembered dungeon depth
  , sdisco   :: !Discoveries  -- ^ remembered item discoveries
  , sfaction :: !FactionDict  -- ^ remembered sides still in game
  , scops    :: Kind.COps     -- ^ remembered content
  , splayer  :: !ActorId      -- ^ selected actor
  , sside    :: !FactionId    -- ^ selected faction
  , sarena   :: !LevelId      -- ^ selected level
  }
  deriving Show

-- | Global, server state.
data StateServer = StateServer
  { sdiscoRev :: !DiscoRev      -- ^ reverse map, used for item creation
  , sflavour  :: !FlavourMap    -- ^ association of flavour to items
  , scounter  :: !Int           -- ^ stores next actor index
  , srandom   :: !R.StdGen      -- ^ current random generator
  , sconfig   :: !Config        -- ^ this game's config (including initial RNG)
  , squit     :: !(Maybe (Bool, Status))  -- ^ cause of game end/exit
  , sdebugSer :: !DebugModeSer  -- ^ debugging mode
  }
  deriving Show

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
data StateClient = StateClient
  { scursor   :: !Cursor        -- ^ cursor position and level to return to
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
    TgtOff       -- ^ not in targeting mode
  | TgtExplicit  -- ^ the player requested targeting mode explicitly
  | TgtAuto      -- ^ the mode was entered (and will be exited) automatically
  deriving (Show, Eq)

-- | Current targeting cursor parameters.
data Cursor = Cursor
  { ctargeting :: !TgtMode  -- ^ targeting mode
  , cposLn     :: !LevelId  -- ^ cursor level
  , cposition  :: !Point    -- ^ cursor coordinates
  , creturnLn  :: !LevelId  -- ^ the level current player resides on
  , ceps       :: !Int      -- ^ a parameter of the tgt digital line
  }
  deriving Show

-- | Current result of the game.
data Status =
    Killed !LevelId  -- ^ the player lost the game on the given level
  | Camping          -- ^ game is supended
  | Victor           -- ^ the player won
  | Restart          -- ^ the player quits and starts a new game
  deriving (Show, Eq, Ord)

data DebugModeSer = DebugModeSer
  { stryFov :: !(Maybe FovMode) }
  deriving Show

data DebugModeCli = DebugModeCli
  { smarkVision :: !Bool
  , smarkSmell  :: !Bool
  , somniscient :: !Bool
  }
  deriving Show

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

defHistory :: IO History
defHistory = do
  dateTime <- getClockTime
  let curDate = MU.Text $ T.pack $ calendarTimeToString $ toUTCTime dateTime
  return $ singletonHistory $ singletonReport
         $ makeSentence ["Player history log started on", curDate]

-- | Initial complete global game state.
defStateGlobal :: Dungeon -> Int -> Discoveries
                   -> FactionDict -> Kind.COps -> FactionId -> LevelId
                   -> State
defStateGlobal sdungeon sdepth sdisco sfaction scops sside sarena =
  State
    { splayer = invalidActorId  -- no heroes yet alive
    , ..
    }

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

-- | Initial game client state.
defStateClient :: Point -> LevelId -> StateClient
defStateClient ppos sarena = do
  StateClient
    { scursor   = (Cursor TgtOff sarena ppos sarena 0)
    , starget   = IM.empty
    , srunning  = Nothing
    , sreport   = emptyReport
    , shistory  = emptyHistory
    , slastKey  = Nothing
    , sdebugCli = defDebugModeCli
    }

defDebugModeSer :: DebugModeSer
defDebugModeSer = DebugModeSer
  { stryFov = Nothing }

defDebugModeCli :: DebugModeCli
defDebugModeCli = DebugModeCli
  { smarkVision = False
  , smarkSmell  = False
  , somniscient = False
  }

-- | Tell whether the faction is human-controlled.
isControlledFaction :: State -> FactionId -> Bool
isControlledFaction s fid = isNothing $ gAiSelected $ sfaction s IM.! fid

-- | Tell whether the faction is human-controlled.
isSpawningFaction :: State -> FactionId -> Bool
isSpawningFaction s fid =
  let Kind.Ops{okind} = Kind.cofact (scops s)
      kind = okind $ gkind $ sfaction s IM.! fid
  in fspawn kind > 0

-- | Update cursor parameters within state.
updateCursor :: (Cursor -> Cursor) -> StateClient -> StateClient
updateCursor f s = s { scursor = f (scursor s) }

-- | Update cursor parameters within state.
updateTarget :: ActorId -> (Maybe Target -> Maybe Target) -> StateClient
             -> StateClient
updateTarget actor f s = s { starget = IM.alter f actor (starget s) }

-- | Update item discoveries within state.
updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdisco = f (sdisco s) }

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {sdungeon = f (sdungeon s)}

-- | Get current level from the dungeon data.
getArena :: State -> Level
getArena State{sarena, sdungeon} = sdungeon M.! sarena

-- | Update level data within state.
updateArena :: (Level -> Level) -> State -> State
updateArena f s = updateDungeon (M.adjust f (sarena s)) s

-- | Get current time from the dungeon data.
getTime :: State -> Time
getTime State{sarena, sdungeon} = ltime $ sdungeon M.! sarena

-- | Update time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = updateArena (\lvl@Level{ltime} -> lvl {ltime = f ltime}) s

cycleTryFov :: StateServer -> StateServer
cycleTryFov s@StateServer{sdebugSer=sdebugSer@DebugModeSer{stryFov}} =
  s {sdebugSer = sdebugSer {stryFov = case stryFov of
                               Nothing          -> Just (Digital 100)
                               Just (Digital _) -> Just Permissive
                               Just Permissive  -> Just Shadow
                               Just Shadow      -> Just Blind
                               Just Blind       -> Nothing }}

toggleMarkVision :: StateClient -> StateClient
toggleMarkVision s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkVision}} =
  s {sdebugCli = sdebugCli {smarkVision = not smarkVision}}

toggleMarkSmell :: StateClient -> StateClient
toggleMarkSmell s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkSmell}} =
  s {sdebugCli = sdebugCli {smarkSmell = not smarkSmell}}

toggleOmniscient :: StateClient -> StateClient
toggleOmniscient s@StateClient{sdebugCli=sdebugCli@DebugModeCli{somniscient}} =
  s {sdebugCli = sdebugCli {somniscient = not somniscient}}

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
    put scursor
    put starget
    put srunning
    put sreport
    put shistory
  get = do
    scursor <- get
    starget <- get
    srunning <- get
    sreport <- get
    shistory <- get
    let slastKey = Nothing
        sdebugCli = defDebugModeCli
    return StateClient{..}

instance Binary TgtMode where
  put TgtOff      = putWord8 0
  put TgtExplicit = putWord8 1
  put TgtAuto     = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      0 -> return TgtOff
      1 -> return TgtExplicit
      2 -> return TgtAuto
      _ -> fail "no parse (TgtMode)"

instance Binary Cursor where
  put (Cursor act cln loc rln eps) = do
    put act
    put cln
    put loc
    put rln
    put eps
  get = do
    act <- get
    cln <- get
    loc <- get
    rln <- get
    eps <- get
    return (Cursor act cln loc rln eps)

-- | The type of na actor target.
data Target =
    TEnemy ActorId Point  -- ^ target an actor with its last seen position
  | TPos Point            -- ^ target a given position
  deriving (Show, Eq)

instance Binary Target where
  put (TEnemy a ll) = putWord8 0 >> put a >> put ll
  put (TPos pos) = putWord8 1 >> put pos
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 TEnemy get get
      1 -> liftM TPos get
      _ -> fail "no parse (Target)"

instance Binary Status where
  put (Killed ln) = putWord8 0 >> put ln
  put Camping     = putWord8 1
  put Victor      = putWord8 2
  put Restart     = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap Killed get
      1 -> return Camping
      2 -> return Victor
      3 -> return Restart
      _ -> fail "no parse (Status)"

-- TODO: probably move somewhere (Level?)
-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the targeting mode.
lookAt :: Kind.COps  -- ^ game content
       -> Bool       -- ^ detailed?
       -> Bool       -- ^ can be seen right now?
       -> State      -- ^ game state
       -> Point      -- ^ position to describe
       -> Text       -- ^ an extra sentence to print
       -> Text
lookAt Kind.COps{coitem, cotile=Kind.Ops{oname}} detailed canSee s loc msg
  | detailed =
    let tile = lvl `at` loc
    in makeSentence [MU.Text $ oname tile] <+> msg <+> isd
  | otherwise = msg <+> isd
 where
  lvl = getArena s
  is  = lvl `atI` loc
  prefixSee = MU.Text $ if canSee then "you see" else "you remember"
  nWs = partItemNWs coitem (sdisco s)
  isd = case is of
          [] -> ""
          _ | length is <= 2 ->
            makeSentence [prefixSee, MU.WWandW $ map nWs is]
          _ | detailed -> "Objects:"
          _ -> "Objects here."
