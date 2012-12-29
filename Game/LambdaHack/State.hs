{-# LANGUAGE OverloadedStrings #-}
-- | Game state and persistent player cli types and operations.
module Game.LambdaHack.State
  ( -- * Game state
    TgtMode(..), Cursor(..), Status(..)
  , State(..), defaultStateGlobal, defaultStateLocal
  , StateServer(..), defaultStateServer
  , StateClient(..), defaultStateClient, defaultHistory
    -- * Accessor
  , slevel, stime
    -- * State update
  , updateCursor, updateTime, updateDiscoveries, updateLevel, updateDungeon
    -- * Textual description
  , lookAt
    -- * Debug flags
  , DebugMode(..), cycleMarkVision, toggleOmniscient
  ) where

import Data.Binary
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Actor
import Game.LambdaHack.Config
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

-- | View on game state.
data State = State
  { sdungeon :: !Dungeon      -- ^ remembered dungeon
  , sdepth   :: !Int          -- ^ remembered dungeon depth
  , sdisco   :: !Discoveries  -- ^ remembered item discoveries
  , sfaction   :: !FactionDict  -- ^ remembered sides still in game
  , scops    :: Kind.COps     -- ^ remembered content
  , splayer  :: !ActorId      -- ^ selected actor
  , sside :: !FactionId    -- ^ selected faction
  , slid     :: !LevelId      -- ^ selected level
  }
  deriving Show

-- | Global, server state.
data StateServer = StateServer
  { sdiscoRev :: !DiscoRev    -- ^ reverse map, used for item creation
  , sflavour  :: !FlavourMap  -- ^ association of flavour to items
  , scounter  :: !Int         -- ^ stores next actor index
  , srandom   :: !R.StdGen    -- ^ current random generator
  , sconfig   :: !Config      -- ^ this game's config (including initial RNG)
  , squit     :: !(Maybe (Bool, Status))  -- ^ cause of game end/exit
  }
  deriving Show

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
data StateClient = StateClient
  { scursor  :: !Cursor        -- ^ cursor location and level to return to
  , sreport  :: !Report        -- ^ current messages
  , shistory :: !History       -- ^ history of messages
  , slastKey :: !(Maybe K.KM)  -- ^ last command key pressed
  , sdebug   :: !DebugMode     -- ^ debugging mode
  }

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
  { ctargeting :: !TgtMode          -- ^ targeting mode
  , clocLn     :: !LevelId  -- ^ cursor level
  , clocation  :: !Point            -- ^ cursor coordinates
  , creturnLn  :: !LevelId  -- ^ the level current player resides on
  , ceps       :: !Int              -- ^ a parameter of the tgt digital line
  }
  deriving Show

-- | Current result of the game.
data Status =
    Killed !LevelId  -- ^ the player lost the game on the given level
  | Camping                  -- ^ game is supended
  | Victor                   -- ^ the player won
  | Restart                  -- ^ the player quits and starts a new game
  deriving (Show, Eq, Ord)

data DebugMode = DebugMode
  { smarkVision :: !(Maybe FovMode)
  , somniscient :: !Bool
  }
  deriving Show

-- | Get current level from the dungeon data.
slevel :: State -> Level
slevel State{slid, sdungeon} = sdungeon M.! slid

-- TODO: add a flag 'fresh' and when saving levels, don't save
-- and when loading regenerate this level.
unknownLevel :: Kind.Ops TileKind -> X -> Y
             -> Text -> (Point, Point) -> Int
             -> Level
unknownLevel Kind.Ops{ouniqGroup} lxsize lysize ldesc lstairs lclear =
  let unknownId = ouniqGroup "unknown space"
  in Level { lactor = IM.empty
           , linv = IM.empty
           , litem = IM.empty
           , lmap = unknownTileMap unknownId lxsize lysize
           , lxsize = lxsize
           , lysize = lysize
           , lsmell = IM.empty
           , ldesc
           , lstairs
           , lseen = 0
           , lclear
           , ltime = timeZero
           , lsecret = IM.empty
           }

unknownTileMap :: Kind.Id TileKind -> Int -> Int -> TileMap
unknownTileMap unknownId cxsize cysize =
  let bounds = (origin, toPoint cxsize $ PointXY (cxsize - 1, cysize - 1))
  in Kind.listArray bounds (repeat unknownId)

-- | Get current time from the dungeon data.
stime :: State -> Time
stime State{slid, sdungeon} = ltime $ sdungeon M.! slid

defaultHistory :: IO History
defaultHistory = do
  dateTime <- getClockTime
  let curDate = MU.Text $ T.pack $ calendarTimeToString $ toUTCTime dateTime
  return $ singletonHistory $ singletonReport
         $ makeSentence ["Player cli started on", curDate]

-- | Initial complete global game state.
defaultStateGlobal :: Dungeon -> Int -> Discoveries
                   -> FactionDict -> Kind.COps -> FactionId -> LevelId
                   -> State
defaultStateGlobal sdungeon sdepth sdisco sfaction scops sside slid =
  State
    { splayer = 0 -- hack: the hero is not yet alive
    , ..
    }

-- | Initial per-faction local game state.
defaultStateLocal :: Dungeon
                  -> Int -> Discoveries -> FactionDict
                  -> Kind.COps -> FactionId -> LevelId
                  -> State
defaultStateLocal globalDungeon
                  sdepth sdisco sfaction
                  scops@Kind.COps{cotile} sside slid = do
  State
    { splayer  = 0 -- hack: the hero is not yet alive
    , sdungeon =
      M.map (\Level{lxsize, lysize, ldesc, lstairs, lclear} ->
              unknownLevel cotile lxsize lysize ldesc lstairs lclear)
            globalDungeon
    , ..
    }

-- | Initial game server state.
defaultStateServer :: DiscoRev -> FlavourMap -> R.StdGen -> Config
                   -> StateServer
defaultStateServer sdiscoRev sflavour srandom sconfig =
  StateServer
    { scounter = 0
    , squit    = Nothing
    , ..
    }

-- | Initial game client state.
defaultStateClient :: Point -> LevelId -> History -> StateClient
defaultStateClient ploc slid shistory = do
  StateClient
    { scursor  = (Cursor TgtOff slid ploc slid 0)
    , sreport  = emptyReport
    , shistory
    , slastKey = Nothing
    , sdebug   = defaultDebugMode
    }

defaultDebugMode :: DebugMode
defaultDebugMode = DebugMode
  { smarkVision = Nothing
  , somniscient = False
  }

-- | Update cursor parameters within state.
updateCursor :: (Cursor -> Cursor) -> StateClient -> StateClient
updateCursor f s = s { scursor = f (scursor s) }

-- | Update time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = updateLevel (\lvl@Level{ltime} -> lvl {ltime = f ltime}) s

-- | Update item discoveries within state.
updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdisco = f (sdisco s) }

-- | Update level data within state.
updateLevel :: (Level -> Level) -> State -> State
updateLevel f s = updateDungeon (M.adjust f (slid s)) s

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {sdungeon = f (sdungeon s)}

cycleMarkVision :: StateClient -> StateClient
cycleMarkVision s@StateClient{sdebug=sdebug@DebugMode{smarkVision}} =
  s {sdebug = sdebug {smarkVision = case smarkVision of
                        Nothing          -> Just (Digital 100)
                        Just (Digital _) -> Just Permissive
                        Just Permissive  -> Just Shadow
                        Just Shadow      -> Just Blind
                        Just Blind       -> Nothing }}

toggleOmniscient :: StateClient -> StateClient
toggleOmniscient s@StateClient{sdebug=sdebug@DebugMode{somniscient}} =
  s {sdebug = sdebug {somniscient = not somniscient}}

instance Binary State where
  put State{..} = do
    put sdungeon
    put sdepth
    put sdisco
    put sfaction
    put splayer
    put sside
    put slid
  get = do
    sdungeon <- get
    sdepth <- get
    sdisco <- get
    sfaction <- get
    splayer <- get
    sside <- get
    slid <- get
    let scops = undefined  -- overwritten by recreated cops
    return State{..}

instance Binary StateClient where
  put StateClient{..} = do
    put scursor
    put sreport
    put shistory
  get = do
    scursor <- get
    sreport  <- get
    shistory <- get
    let slastKey = Nothing
        sdebug = defaultDebugMode
    return StateClient{..}

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
    return StateServer{..}

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
-- explored location. Mute for unknown locations.
-- The detailed variant is for use in the targeting mode.
lookAt :: Kind.COps  -- ^ game content
       -> Bool       -- ^ detailed?
       -> Bool       -- ^ can be seen right now?
       -> State      -- ^ game state
       -> Point      -- ^ location to describe
       -> Text       -- ^ an extra sentence to print
       -> Text
lookAt Kind.COps{coitem, cotile=Kind.Ops{oname}} detailed canSee s loc msg
  | detailed =
    let tile = lvl `at` loc
    in makeSentence [MU.Text $ oname tile] <+> msg <+> isd
  | otherwise = msg <+> isd
 where
  lvl = slevel s
  is  = lvl `atI` loc
  prefixSee = MU.Text $ if canSee then "you see" else "you remember"
  nWs = partItemNWs coitem (sdisco s)
  isd = case is of
          [] -> ""
          _ | length is <= 2 ->
            makeSentence [prefixSee, MU.WWandW $ map nWs is]
          _ | detailed -> "Objects:"
          _ -> "Objects here."
