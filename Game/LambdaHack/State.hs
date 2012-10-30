-- | Game state and persistent player diary types and operations.
module Game.LambdaHack.State
  ( -- * Game state
    State(..), TgtMode(..), Cursor(..), Status(..)
    -- * Accessor
  , slevel, stime
    -- * Constructor
  , defaultState
    -- * State update
  , updateCursor, updateTime, updateDiscoveries, updateLevel, updateDungeon
    -- * Player diary
  , Diary(..), defaultDiary
    -- * Debug flags
  , DebugMode(..), cycleMarkVision, toggleOmniscient
  ) where

import qualified Data.Set as S
import Data.Binary
import qualified Game.LambdaHack.Config as Config
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Actor
import Game.LambdaHack.Point
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Dungeon as Dungeon
import Game.LambdaHack.Item
import Game.LambdaHack.Msg
import Game.LambdaHack.FOV
import Game.LambdaHack.Time
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Content.FactionKind

-- | The diary contains all the player data that carries over
-- from game to game, even across playing sessions. That includes
-- the last message, previous messages and otherwise recorded
-- history of past games. This can be extended with other data and used for
-- calculating player achievements, unlocking advanced game features and
-- for general data mining, e.g., augmenting AI or procedural content
-- generation.
data Diary = Diary
  { sreport  :: Report
  , shistory :: History
  }

-- TODO: stakeTime and squit are also temporary, move them to
-- DungeonPerception and rename it to TurnCache, if more appear, e.g. AI stuff.
-- | The state of a single game that can be saved and restored.
-- It's completely disregarded and reset when a new game is started.
-- In practice, we maintain some extra state (DungeonPerception),
-- but it's only temporary, existing for a single turn and then invalidated.
data State = State
  { splayer  :: ActorId      -- ^ represents the player-controlled actor
  , scursor  :: Cursor       -- ^ cursor location and level to return to
  , sflavour :: FlavourMap   -- ^ association of flavour to items
  , sdisco   :: Discoveries  -- ^ items (kinds) that have been discovered
  , sdungeon :: Dungeon.Dungeon  -- ^ all dungeon levels
  , slid     :: Dungeon.LevelId  -- ^ identifier of the current level
  , scounter :: Int          -- ^ stores next actor index
  , srandom  :: R.StdGen     -- ^ current random generator
  , sconfig  :: Config.CP    -- ^ this game's config (including initial RNG)
  , stakeTime :: Maybe Bool  -- ^ last command unexpectedly took some time
  , squit    :: Maybe (Bool, Status)  -- ^ cause of game end/exit
  , sfaction :: Kind.Id FactionKind   -- ^ our faction
  , sdebug   :: DebugMode    -- ^ debugging mode
  }
  deriving Show

-- | Current targeting mode of the player.
data TgtMode =
    TgtOff       -- ^ not in targeting mode
  | TgtExplicit  -- ^ the player requested targeting mode explicitly
  | TgtAuto      -- ^ the mode was entered (and will be exited) automatically
  deriving (Show, Eq)

-- | Current targeting cursor parameters.
data Cursor = Cursor
  { ctargeting :: TgtMode          -- ^ targeting mode
  , clocLn     :: Dungeon.LevelId  -- ^ cursor level
  , clocation  :: Point            -- ^ cursor coordinates
  , creturnLn  :: Dungeon.LevelId  -- ^ the level current player resides on
  , ceps       :: Int              -- ^ a parameter of the tgt digital line
  }
  deriving Show

-- | Current result of the game.
data Status =
    Killed !Dungeon.LevelId  -- ^ the player lost the game on the given level
  | Camping                  -- ^ game is supended
  | Victor                   -- ^ the player won
  | Restart                  -- ^ the player quits and starts a new game
  deriving (Show, Eq, Ord)

data DebugMode = DebugMode
  { smarkVision :: Maybe FovMode
  , somniscient :: Bool
  }
  deriving Show

-- | Get current level from the dungeon data.
slevel :: State -> Level
slevel State{slid, sdungeon} = sdungeon Dungeon.! slid

-- | Get current time from the dungeon data.
stime :: State -> Time
stime State{slid, sdungeon} = ltime $ sdungeon Dungeon.! slid

-- | Initial player diary.
defaultDiary :: IO Diary
defaultDiary = do
  dateTime <- getClockTime
  let curDate = calendarTimeToString $ toUTCTime dateTime
  return Diary
    { sreport = emptyReport
    , shistory = singletonHistory $ singletonReport $
                   "Player diary started on " ++ curDate ++ "."
    }

-- | Initial game state.
defaultState :: Config.CP -> Kind.Id FactionKind -> FlavourMap
             -> Dungeon.Dungeon -> Dungeon.LevelId -> Point -> R.StdGen
             -> State
defaultState config sfaction flavour dng lid ploc g =
  State
    0  -- hack: the hero is not yet alive
    (Cursor TgtOff lid ploc lid 0)
    flavour
    S.empty
    dng
    lid
    0
    g
    config
    Nothing
    Nothing
    sfaction
    defaultDebugMode

defaultDebugMode :: DebugMode
defaultDebugMode = DebugMode
  { smarkVision = Nothing
  , somniscient = False
  }

-- | Update cursor parameters within state.
updateCursor :: (Cursor -> Cursor) -> State -> State
updateCursor f s = s { scursor = f (scursor s) }

-- | Update time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = updateLevel (\ lvl@Level{ltime} -> lvl {ltime = f ltime}) s

-- | Update item discoveries within state.
updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdisco = f (sdisco s) }

-- | Update level data within state.
updateLevel :: (Level -> Level) -> State -> State
updateLevel f s = updateDungeon (Dungeon.adjust f (slid s)) s

-- | Update dungeon data within state.
updateDungeon :: (Dungeon.Dungeon -> Dungeon.Dungeon) -> State -> State
updateDungeon f s = s {sdungeon = f (sdungeon s)}

cycleMarkVision :: State -> State
cycleMarkVision s@State{sdebug = sdebug@DebugMode{smarkVision}} =
  s {sdebug = sdebug {smarkVision = case smarkVision of
                        Nothing          -> Just (Digital 100)
                        Just (Digital _) -> Just Permissive
                        Just Permissive  -> Just Shadow
                        Just Shadow      -> Just Blind
                        Just Blind       -> Nothing }}

toggleOmniscient :: State -> State
toggleOmniscient s@State{sdebug = sdebug@DebugMode{somniscient}} =
  s {sdebug = sdebug {somniscient = not somniscient}}

instance Binary Diary where
  put Diary{..} = do
    put sreport
    put shistory
  get = do
    sreport  <- get
    shistory <- get
    return Diary{..}

instance Binary State where
  put (State player cursor flav disco dng lid ct
         g config stakeTime _ sfaction _) = do
    put player
    put cursor
    put flav
    put disco
    put dng
    put lid
    put ct
    put (show g)
    put config
    put stakeTime
    put sfaction
  get = do
    player <- get
    cursor <- get
    flav   <- get
    disco  <- get
    dng    <- get
    lid    <- get
    ct     <- get
    g      <- get
    config   <- get
    stakeTime  <- get
    sfaction <- get
    return
      (State player cursor flav disco dng lid ct (read g) config stakeTime
         Nothing sfaction defaultDebugMode)

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
