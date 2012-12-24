{-# LANGUAGE OverloadedStrings #-}
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
    -- * Textual description
  , lookAt
    -- * Debug flags
  , DebugMode(..), cycleMarkVision, toggleOmniscient
  ) where

import Data.Binary
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Actor
import Game.LambdaHack.Config
import Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Dungeon as Dungeon
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.Time

-- | The diary contains all the player data that carries over
-- from game to game, even across playing sessions. That includes
-- the last message, previous messages and otherwise recorded
-- history of past games. This can be extended with other data and used for
-- calculating player achievements, unlocking advanced game features and
-- for general data mining, e.g., augmenting AI or procedural content
-- generation. @Diary@, even the @shistory@ part, is more than a WriterMonad
-- because it can be displayed and saved at any time.
data Diary = Diary
  { sreport  :: !Report
  , shistory :: !History
  }

-- | The state of a single game that can be saved and restored.
-- It's completely disregarded and reset when a new game is started.
-- In practice, we maintain some extra state (DungeonPerception) elsewhere,
-- but it's only temporary, existing for a single turn and then invalidated.
data State = State
  { splayer   :: !ActorId       -- ^ represents the player-controlled actor
  , scursor   :: !Cursor        -- ^ cursor location and level to return to
  , sflavour  :: !FlavourMap    -- ^ association of flavour to items
  , sdisco    :: !Discoveries   -- ^ items (kinds) that have been discovered
  , sdiscoS   :: !Discoveries   -- ^ all item kinds, as known by the server
  , sdiscoRev :: !DiscoRev   -- ^ reverse map, used for item creation
  , sdungeon  :: !Dungeon.Dungeon  -- ^ all dungeon levels
  , slid      :: !Dungeon.LevelId  -- ^ identifier of the current level
  , scounter  :: !Int           -- ^ stores next actor index
  , srandom   :: !R.StdGen      -- ^ current random generator
  , sconfig   :: !Config        -- ^ this game's config (including initial RNG)
  , squit     :: !(Maybe (Bool, Status))       -- ^ cause of game end/exit
  , sfaction  :: !(Kind.Id FactionKind)        -- ^ our faction
  , slastKey  :: !(Maybe (K.Key, K.Modifier))  -- ^ last command key pressed
  , sdebug    :: !DebugMode     -- ^ debugging mode
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
  { ctargeting :: !TgtMode          -- ^ targeting mode
  , clocLn     :: !Dungeon.LevelId  -- ^ cursor level
  , clocation  :: !Point            -- ^ cursor coordinates
  , creturnLn  :: !Dungeon.LevelId  -- ^ the level current player resides on
  , ceps       :: !Int              -- ^ a parameter of the tgt digital line
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
  { smarkVision :: !(Maybe FovMode)
  , somniscient :: !Bool
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
  let curDate = MU.Text $ T.pack $ calendarTimeToString $ toUTCTime dateTime
  return Diary
    { sreport = emptyReport
    , shistory = singletonHistory $ singletonReport $
                   makeSentence ["Player diary started on", curDate]
    }

-- | Initial game state.
defaultState :: FlavourMap -> Discoveries -> Discoveries -> DiscoRev
             -> Dungeon.Dungeon -> Dungeon.LevelId -> R.StdGen
             -> Config -> Kind.Id FactionKind -> Point -> State
defaultState sflavour sdisco sdiscoS sdiscoRev
             sdungeon slid srandom
             sconfig sfaction ploc =
  State
    { splayer  = 0 -- hack: the hero is not yet alive
    , scursor  = (Cursor TgtOff slid ploc slid 0)
    , scounter = 0
    , squit    = Nothing
    , slastKey = Nothing
    , sdebug   = defaultDebugMode
    , ..
    }

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
  put State{..} = do
    put splayer
    put scursor
    put sflavour
    put sdisco
    put sdiscoS
    put sdiscoRev
    put sdungeon
    put slid
    put scounter
    put (show srandom)
    put sconfig
    put sfaction
  get = do
    splayer <- get
    scursor <- get
    sflavour <- get
    sdisco <- get
    sdiscoS <- get
    sdiscoRev <- get
    sdungeon <- get
    slid <- get
    scounter <- get
    g <- get
    sconfig <- get
    sfaction <- get
    let squit = Nothing
        sdebug = defaultDebugMode
        slastKey = Nothing
        srandom = read g
    return State{..}

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
       -> Level      -- ^ current level
       -> Point      -- ^ location to describe
       -> Text       -- ^ an extra sentence to print
       -> Text
lookAt Kind.COps{coitem, cotile=Kind.Ops{oname}} detailed canSee s lvl loc msg
  | detailed =
    let tile = lvl `rememberAt` loc
    in makeSentence [MU.Text $ oname tile] <+> msg <+> isd
  | otherwise = msg <+> isd
 where
  is  = lvl `rememberAtI` loc
  prefixSee = MU.Text $ if canSee then "you see" else "you remember"
  nWs = partItemNWs coitem (sdisco s)
  isd = case is of
          [] -> ""
          _ | length is <= 3 ->
            makeSentence [prefixSee, MU.WWandW $ map nWs is]
          _ | detailed -> "Objects:"
          _ -> "Objects here."
