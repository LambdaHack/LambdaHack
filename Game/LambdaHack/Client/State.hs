{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Client.State
  ( StateClient(..), defStateClient, defHistory
  , updateTarget, getTarget, updateLeader, sside
  , PathEtc, TgtMode(..), Target(..), RunParams(..), LastRecord
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

import Game.LambdaHack.Client.ConfigUI
import Game.LambdaHack.Client.ItemSlot
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
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
-- Data invariant: if @_sleader@ is @Nothing@ then so is @srunning@.
data StateClient = StateClient
  { stgtMode     :: !(Maybe TgtMode)  -- ^ targeting mode
  , scursor      :: !Target           -- ^ the common, cursor target
  , seps         :: !Int              -- ^ a parameter of the tgt digital line
  , stargetD     :: !(EM.EnumMap ActorId (Target, Maybe PathEtc))
                                   -- ^ targets of our actors in the dungeon
  , sexplored    :: !(ES.EnumSet LevelId)
                                   -- ^ the set of fully explored levels
  , sbfsD        :: !(EM.EnumMap ActorId
                        ( PointArray.Array BfsDistance
                        , Point, Int, Maybe [Point]) )
                                   -- ^ pathfinding distances for our actors
                                   --   and paths to their targets, if any
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
  , slastRecord  :: !LastRecord    -- ^ state of key sequence recording
  , slastPlay    :: ![K.KM]        -- ^ state of key sequence playback
  , slastCmd     :: !(Maybe CmdTakeTimeSer)
                                   -- ^ last command sent to the server
  , swaitTimes   :: !Int           -- ^ player just waited this many times
  , _sleader     :: !(Maybe ActorId)
                                   -- ^ current picked party leader
  , _sside       :: !FactionId     -- ^ faction controlled by the client
  , squit        :: !Bool          -- ^ exit the game loop
  , sisAI        :: !Bool          -- ^ whether it's an AI client
  , smarkVision  :: !Bool          -- ^ mark leader and party FOV
  , smarkSmell   :: !Bool          -- ^ mark smell, if the leader can smell
  , smarkSuspect :: !Bool          -- ^ mark suspect features
  , scurDifficulty :: !Int         -- ^ current game difficulty level
  , sslots       :: !ItemSlots     -- ^ map from slots to items
  , sfreeSlot    :: !SlotChar      -- ^ first free slot
  , sdebugCli    :: !DebugModeCli  -- ^ client debugging mode
  }
  deriving Show

type PathEtc = ([Point], (Point, Int))

-- | Current targeting mode of a client.
newtype TgtMode = TgtMode { tgtLevelId :: LevelId }
  deriving (Show, Eq, Binary)

-- | The type of na actor target.
data Target =
    TEnemy !ActorId !Bool
    -- ^ target an actor; cycle only trough seen foes, unless the flag is set
  | TEnemyPos !ActorId !LevelId !Point !Bool
    -- ^ last seen position of the targeted actor
  | TPoint !LevelId !Point              -- ^ target a concrete spot
  | TVector !Vector                     -- ^ target position relative to actor
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

type LastRecord = ( [K.KM]  -- accumulated keys of the current command
                  , [K.KM]  -- keys of the rest of the recorded command batch
                  , Int     -- commands left to record for this batch
                  )

-- | Initial game client state.
defStateClient :: History -> ConfigUI -> FactionId -> Bool
               -> StateClient
defStateClient shistory sconfigUI _sside sisAI =
  StateClient
    { stgtMode = Nothing
    , scursor = if sisAI
                then TVector $ Vector 30000 30000  -- invalid
                else TVector $ Vector 1 1  -- a step south-east
    , seps = 0
    , stargetD = EM.empty
    , sexplored = ES.empty
    , sbfsD = EM.empty
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
    , slastRecord = ([], [], 0)
    , slastPlay = []
    , slastCmd = Nothing
    , swaitTimes = 0
    , _sleader = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , sisAI
    , smarkVision = False
    , smarkSmell = False
    , smarkSuspect = False
    , scurDifficulty = difficultyDefault
    , sslots = EM.empty
    , sfreeSlot = SlotChar 'a'
    , sdebugCli = defDebugModeCli
    }

defHistory :: IO History
defHistory = do
  dateTime <- getClockTime
  let curDate = MU.Text $ T.pack $ calendarTimeToString $ toUTCTime dateTime
  return $! singletonHistory $ singletonReport
         $! makeSentence ["Human history log started on", curDate]

-- | Update target parameters within client state.
updateTarget :: ActorId -> (Maybe Target -> Maybe Target) -> StateClient
             -> StateClient
updateTarget aid f cli =
  let f2 tp = case f $ fmap fst tp of
        Nothing -> Nothing
        Just tgt -> Just (tgt, Nothing)  -- reset path
  in cli {stargetD = EM.alter f2 aid (stargetD cli)}

-- | Get target parameters from client state.
getTarget :: ActorId -> StateClient -> Maybe Target
getTarget aid cli = fmap fst $ EM.lookup aid $ stargetD cli

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
    put sexplored
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
    put scurDifficulty
    put sslots
    put sfreeSlot
    put sdebugCli  -- TODO: this is overwritten at once
  get = do
    stgtMode <- get
    scursor <- get
    seps <- get
    stargetD <- get
    sexplored <- get
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
    scurDifficulty <- get
    sslots <- get
    sfreeSlot <- get
    sdebugCli <- get
    let sbfsD = EM.empty
        sfper = EM.empty
        srandom = read g
        slastKey = Nothing
        slastRecord = ([], [], 0)
        slastPlay = []
        slastCmd = Nothing
        swaitTimes = 0
        squit = False
        sconfigUI = undefined
    return $! StateClient{..}

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
    return $! RunParams{..}

instance Binary Target where
  put (TEnemy a permit) = putWord8 0 >> put a >> put permit
  put (TEnemyPos a lid p permit) =
    putWord8 1 >> put a >> put lid >> put p >> put permit
  put (TPoint lid p) = putWord8 2 >> put lid >> put p
  put (TVector v) = putWord8 3 >> put v
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 TEnemy get get
      1 -> liftM4 TEnemyPos get get get get
      2 -> liftM2 TPoint get get
      3 -> liftM TVector get
      _ -> fail "no parse (Target)"
