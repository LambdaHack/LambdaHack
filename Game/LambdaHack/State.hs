module Game.LambdaHack.State where

import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Binary
import qualified Game.LambdaHack.Config as Config
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Actor
import Game.LambdaHack.PointXY
import Game.LambdaHack.Loc
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Dungeon as Dungeon
import Game.LambdaHack.Item
import Game.LambdaHack.Msg
import Game.LambdaHack.FOV

-- | The diary contains all the player data that carries over from game to game.
-- That includes the last message, previous messages and otherwise recorded
-- history of past games. This can be used for calculating player
-- achievements, unlocking advanced game features and general data mining.
data Diary = Diary
  { smsg         :: Msg
  , shistory     :: [Msg]
  }

-- | The 'State' contains all the single game state that has to be saved.
-- In practice, we maintain extra state, but that state is state
-- accumulated during a turn or relevant only to the current session.
data State = State
  { splayer      :: ActorId      -- ^ represents the player-controlled actor
  , scursor      :: Cursor       -- ^ cursor location and level to return to
  , ssensory     :: SensoryMode  -- ^ debug only
  , sdisplay     :: DisplayMode  -- ^ debug only
  , stime        :: Time
  , sflavour     :: FlavourMap   -- ^ association of flavour to items
  , sdisco       :: Discoveries  -- ^ items (kinds) that have been discovered
  , sdungeon     :: Dungeon.Dungeon  -- ^ all dungeon levels
  , slid         :: Dungeon.LevelId
  , scounter     :: (Int, Int)   -- ^ stores next hero index and monster index
  , sparty       :: IS.IntSet    -- ^ heroes in the party
  , srandom      :: R.StdGen     -- ^ current random generator
  , sconfig      :: Config.CP
  }
  deriving Show

data TgtMode =
    TgtOff     -- ^ not in targeting mode
  | TgtPlayer  -- ^ the player requested targeting mode explicitly
  | TgtAuto    -- ^ the mode was entered (and will be exited) automatically
  deriving (Show, Eq)

data Cursor = Cursor
  { ctargeting :: TgtMode  -- ^ targeting mode
  , clocLn     :: Dungeon.LevelId  -- ^ cursor level
  , clocation  :: Loc      -- ^ cursor coordinates
  , creturnLn  :: Dungeon.LevelId  -- ^ the level current player resides on
  }
  deriving Show

data SensoryMode =
    Implicit
  | Vision FovMode
  deriving Show

data DisplayMode =
    Normal
  | Omniscient
  deriving Show

slevel :: State -> Level
slevel State{slid, sdungeon} = sdungeon Dungeon.! slid

-- TODO: add date.
defaultDiary :: IO Diary
defaultDiary = do
  curDate <- getClockTime
  let time = calendarTimeToString $ toUTCTime $ curDate
  return $ Diary
    { smsg = ""
    , shistory = ["Player diary started on " ++ time ++ "."]
    }

defaultState :: Config.CP -> FlavourMap -> Dungeon.Dungeon -> Dungeon.LevelId
             -> Loc -> R.StdGen -> State
defaultState config flavour dng lid ploc g =
  State
    (AHero 0)  -- hack: the hero is not yet alive
    (Cursor TgtOff lid ploc lid)
    Implicit Normal
    0
    flavour
    S.empty
    dng
    lid
    (0, 0)
    IS.empty
    g
    config

updateCursor :: (Cursor -> Cursor) -> State -> State
updateCursor f s = s { scursor = f (scursor s) }

updateTime :: (Time -> Time) -> State -> State
updateTime f s = s { stime = f (stime s) }

updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdisco = f (sdisco s) }

updateLevel :: (Level -> Level) -> State -> State
updateLevel f s = updateDungeon (Dungeon.adjust f (slid s)) s

updateDungeon :: (Dungeon.Dungeon -> Dungeon.Dungeon) -> State -> State
updateDungeon f s = s {sdungeon = f (sdungeon s)}

toggleVision :: State -> State
toggleVision s = s { ssensory = case ssensory s of
                        Implicit           -> Vision (Digital 100)
                        Vision (Digital _) -> Vision Permissive
                        Vision Permissive  -> Vision Shadow
                        Vision Shadow      -> Vision Blind
                        Vision Blind       -> Implicit }

toggleOmniscient :: State -> State
toggleOmniscient s = s { sdisplay = case sdisplay s of
                            Omniscient -> Normal
                            Normal     -> Omniscient }

instance Binary Diary where
  put Diary{..} = do
    put smsg
    put shistory
  get = do
    smsg     <- get
    shistory <- get
    return Diary{..}

instance Binary State where
  put (State player cursor _ _ time flav disco dng lid ct
       party g config) = do
    put player
    put cursor
    put time
    put flav
    put disco
    put dng
    put lid
    put ct
    put party
    put (show g)
    put config
  get = do
    player <- get
    cursor <- get
    time   <- get
    flav   <- get
    disco  <- get
    dng    <- get
    lid    <- get
    ct     <- get
    party  <- get
    g      <- get
    config <- get
    return
      (State player cursor Implicit Normal time flav disco dng lid ct
       party (read g) config)

instance Binary Cursor where
  put (Cursor act cln loc rln) = do
    put act
    put cln
    put loc
    put rln
  get = do
    act <- get
    cln <- get
    loc <- get
    rln <- get
    return (Cursor act cln loc rln)

instance Binary TgtMode where
  put TgtOff    = putWord8 0
  put TgtPlayer = putWord8 1
  put TgtAuto   = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      0 -> return TgtOff
      1 -> return TgtPlayer
      2 -> return TgtAuto
      _ -> fail "no parse (TgtMode)"
