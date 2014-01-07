-- | Server and client game state types and operations.
module Game.LambdaHack.Client.State
  ( StateClient(..), defStateClient, defHistory
  , updateTarget, getTarget, updateLeader, sside
  , TgtMode(..), Target(..), RunParams(..), LastSeq(..)
  , toggleMarkVision, toggleMarkSmell, toggleMarkSuspect
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
-- Data invariant: if @_sleader@ is @Nothing@ then so is @srunning@.
data StateClient = StateClient
  { stgtMode     :: !(Maybe TgtMode)  -- ^ targeting mode
  , scursor      :: !(Maybe Point)    -- ^ cursor coordinates
  , seps         :: !Int              -- ^ a parameter of the tgt digital line
  , stargetD     :: !(EM.EnumMap ActorId Target)
                                   -- ^ targets of our actors in the dungeon
  , sselected    :: !(ES.EnumSet ActorId)
                                   -- ^ the set of currently selected actors
  , srunning     :: !(Maybe RunParams)
                                   -- ^ parameters of the current run, if any
  , sreport      :: !Report        -- ^ current messages
  , shistory     :: !History       -- ^ history of messages
  , sundo        :: ![Atomic]      -- ^ atomic commands performed to date
  , sdisco       :: !Discovery     -- ^ remembered item discoveries
  , sfper        :: !FactionPers   -- ^ faction perception indexed by levels
  , srandom      :: !R.StdGen      -- ^ current random generator
  , sconfigUI    :: ConfigUI       -- ^ client config (including initial RNG)
  , slastKey     :: !(Maybe K.KM)  -- ^ last command key pressed
  , slastSeq     :: !LastSeq       -- ^ state of key sequence recording/playing
  , slastCmd     :: !(Maybe CmdTakeTimeSer)
                                   -- ^ last command sent to the server
  , _sleader     :: !(Maybe ActorId)
                                   -- ^ current picked party leader
  , _sside       :: !FactionId     -- ^ faction controlled by the client
  , squit        :: !Bool          -- ^ exit the game loop
  , sisAI        :: !Bool          -- ^ whether it's an AI client
  , smarkVision  :: !Bool          -- ^ mark leader and party FOV
  , smarkSmell   :: !Bool          -- ^ mark smell, if the leader can smell
  , smarkSuspect :: !Bool          -- ^ mark suspect features
  , sdifficulty  :: !Int           -- ^ current game difficulty level
  , sdebugCli    :: !DebugModeCli  -- ^ client debugging mode
  }
  deriving Show

-- | Current targeting mode of a client.
data TgtMode =
    TgtExplicit { tgtLevelId :: !LevelId }
      -- ^ the player requested targeting mode explicitly
  | TgtAuto     { tgtLevelId :: !LevelId }
      -- ^ the mode was entered (and will be exited) automatically
  deriving (Show, Eq)

-- | The type of na actor target.
data Target =
    TEnemy !ActorId !Point  -- ^ target an actor with its last seen position
  | TPos !Point             -- ^ target a given position
  deriving (Show, Eq)

-- | Parameters of the current run.
data RunParams = RunParams
  { runLeader  :: !ActorId         -- ^ the original leader from run start
  , runMembers :: ![ActorId]       -- ^ the list of actors that take part
  , runDist    :: !Int             -- ^ distance of the run so far
                                   --   (plus one, if multiple runners)
  , runStopMsg :: !(Maybe Text)    -- ^ message with the next stop reason
  , runInitDir :: !(Maybe Vector)  -- ^ the direction of the initial step
  }
  deriving (Show)

data LastSeq =
    LPlayBack
      { seqMacro :: ![K.KM]
      }
  | LRecord
      { seqCurrent  :: ![K.KM]
      , seqPrevious :: ![K.KM]
      , seqRecordN  :: !Int
      }
  deriving Show

-- | Initial game client state.
defStateClient :: History -> ConfigUI -> FactionId -> Bool
               -> StateClient
defStateClient shistory sconfigUI _sside sisAI =
  StateClient
    { stgtMode = Nothing
    , scursor = Nothing
    , seps = 0
    , stargetD = EM.empty
    , sselected = ES.empty
    , srunning = Nothing
    , sreport = emptyReport
    , shistory
    , sundo = []
    , sdisco = EM.empty
    , sfper = EM.empty
    , sconfigUI
    , srandom = R.mkStdGen 42  -- will be set later
    , slastKey = Nothing
    , slastSeq = LRecord [] [] 0
    , slastCmd = Nothing
    , _sleader = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , sisAI
    , smarkVision = False
    , smarkSmell = False
    , smarkSuspect = False
    , sdifficulty = 0
    , sdebugCli = defDebugModeCli
    }

defHistory :: IO History
defHistory = do
  dateTime <- getClockTime
  let curDate = MU.Text $ T.pack $ calendarTimeToString $ toUTCTime dateTime
  return $ singletonHistory $ singletonReport
         $ makeSentence ["Human history log started on", curDate]

-- | Update target parameters within client state.
updateTarget :: ActorId -> (Maybe Target -> Maybe Target) -> StateClient
             -> StateClient
updateTarget aid f cli = cli {stargetD = EM.alter f aid (stargetD cli)}

-- | Get target parameters from client state.
getTarget :: ActorId -> StateClient -> Maybe Target
getTarget aid cli = EM.lookup aid $ stargetD cli

-- | Update picked leader within state. Verify actor's faction.
updateLeader :: ActorId -> State -> StateClient -> StateClient
updateLeader leader s cli =
  let side1 = bfid $ getActorBody leader s
      side2 = sside cli
  in assert (side1 == side2 `blame` "enemy actor becomes our leader"
                            `twith` (side1, side2, leader, s))
     $ cli {_sleader = Just leader}

sside :: StateClient -> FactionId
sside = _sside

toggleMarkVision :: StateClient -> StateClient
toggleMarkVision s@StateClient{smarkVision} = s {smarkVision = not smarkVision}

toggleMarkSmell :: StateClient -> StateClient
toggleMarkSmell s@StateClient{smarkSmell} = s {smarkSmell = not smarkSmell}

toggleMarkSuspect :: StateClient -> StateClient
toggleMarkSuspect s@StateClient{smarkSuspect} =
  s {smarkSuspect = not smarkSuspect}

instance Binary StateClient where
  put StateClient{..} = do
    put stgtMode
    put scursor
    put seps
    put stargetD
    put sselected
    put srunning
    put sreport
    put shistory
    put sundo
    put sdisco
    put (show srandom)
    put _sleader
    put _sside
    put sisAI
    put smarkVision
    put smarkSmell
    put smarkSuspect
    put sdifficulty
    put sdebugCli
  get = do
    stgtMode <- get
    scursor <- get
    seps <- get
    stargetD <- get
    sselected <- get
    srunning <- get
    sreport <- get
    shistory <- get
    sundo <- get
    sdisco <- get
    g <- get
    _sleader <- get
    _sside <- get
    sisAI <- get
    smarkVision <- get
    smarkSmell <- get
    smarkSuspect <- get
    sdifficulty <- get
    sdebugCli <- get
    let sfper = EM.empty
        srandom = read g
        slastKey = Nothing
        slastSeq = LRecord [] [] 0
        slastCmd = Nothing
        squit = False
        sconfigUI = undefined
    return StateClient{..}

instance Binary RunParams where
  put RunParams{..} = do
    put runLeader
    put runMembers
    put runDist
    put runStopMsg
    put runInitDir
  get = do
    runLeader <- get
    runMembers <- get
    runDist<- get
    runStopMsg <- get
    runInitDir <- get
    return RunParams{..}

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
