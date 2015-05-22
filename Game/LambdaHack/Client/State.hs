{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Client.State
  ( StateClient(..), defStateClient, defaultHistory
  , updateTarget, getTarget, updateLeader, sside
  , PathEtc, TgtMode(..), RunParams(..), LastRecord, EscAI(..)
  , toggleMarkVision, toggleMarkSmell, toggleMarkSuspect
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
-- Data invariant: if @_sleader@ is @Nothing@ then so is @srunning@.
data StateClient = StateClient
  { stgtMode     :: !(Maybe TgtMode)
                                   -- ^ targeting mode
  , scursor      :: !Target        -- ^ the common, cursor target
  , seps         :: !Int           -- ^ a parameter of the tgt digital line
  , stargetD     :: !(EM.EnumMap ActorId (Target, Maybe PathEtc))
                                   -- ^ targets of our actors in the dungeon
  , sexplored    :: !(ES.EnumSet LevelId)
                                   -- ^ the set of fully explored levels
  , sbfsD        :: !(EM.EnumMap ActorId
                        ( Bool, PointArray.Array BfsDistance
                        , Point, Int, Maybe [Point]) )
                                   -- ^ pathfinding distances for our actors
                                   --   and paths to their targets, if any
  , sselected    :: !(ES.EnumSet ActorId)
                                   -- ^ the set of currently selected actors
  , srunning     :: !(Maybe RunParams)
                                   -- ^ parameters of the current run, if any
  , sreport      :: !Report        -- ^ current messages
  , shistory     :: !History       -- ^ history of messages
  , sdisplayed   :: !(EM.EnumMap LevelId Time)
                                   -- ^ moves are displayed up to this time
  , sundo        :: ![CmdAtomic]   -- ^ atomic commands performed to date
  , sdiscoKind   :: !DiscoveryKind    -- ^ remembered item discoveries
  , sdiscoEffect :: !DiscoveryEffect  -- ^ remembered effects&Co of items
  , sfper        :: !FactionPers   -- ^ faction perception indexed by levels
  , srandom      :: !R.StdGen      -- ^ current random generator
  , slastKM      :: !K.KM          -- ^ last issued key command
  , slastRecord  :: !LastRecord    -- ^ state of key sequence recording
  , slastPlay    :: ![K.KM]        -- ^ state of key sequence playback
  , slastLost    :: !(ES.EnumSet ActorId)
                                   -- ^ actors that just got out of sight
  , swaitTimes   :: !Int           -- ^ player just waited this many times
  , _sleader     :: !(Maybe ActorId)
                                   -- ^ current picked party leader
  , _sside       :: !FactionId     -- ^ faction controlled by the client
  , squit        :: !Bool          -- ^ exit the game loop
  , sisAI        :: !Bool          -- ^ whether it's an AI client
  , smarkVision  :: !Bool          -- ^ mark leader and party FOV
  , smarkSmell   :: !Bool          -- ^ mark smell, if the leader can smell
  , smarkSuspect :: !Bool          -- ^ mark suspect features
  , scurDiff     :: !Int           -- ^ current game difficulty level
  , snxtDiff     :: !Int           -- ^ next game difficulty level
  , sslots       :: !ItemSlots     -- ^ map from slots to items
  , slastSlot    :: !SlotChar      -- ^ last used slot
  , slastStore   :: !CStore        -- ^ last used store
  , sescAI       :: !EscAI         -- ^ just canceled AI control with ESC
  , sdebugCli    :: !DebugModeCli  -- ^ client debugging mode
  }
  deriving Show

type PathEtc = ([Point], (Point, Int))

-- | Current targeting mode of a client.
newtype TgtMode = TgtMode { tgtLevelId :: LevelId }
  deriving (Show, Eq, Binary)

-- | Parameters of the current run.
data RunParams = RunParams
  { runLeader  :: !ActorId         -- ^ the original leader from run start
  , runMembers :: ![ActorId]       -- ^ the list of actors that take part
  , runInitial :: !Bool            -- ^ initial run continuation by any
                                   --   run participant, including run leader
  , runStopMsg :: !(Maybe Text)    -- ^ message with the next stop reason
  , runWaiting :: !Int             -- ^ waiting for others to move out of the way
  }
  deriving (Show)

type LastRecord = ( [K.KM]  -- accumulated keys of the current command
                  , [K.KM]  -- keys of the rest of the recorded command batch
                  , Int     -- commands left to record for this batch
                  )

data EscAI = EscAINothing | EscAIStarted | EscAIMenu | EscAIExited
  deriving (Show, Eq)

-- | Initial game client state.
defStateClient :: History -> Report -> FactionId -> Bool -> StateClient
defStateClient shistory sreport _sside sisAI =
  StateClient
    { stgtMode = Nothing
    , scursor = if sisAI
                then TVector $ Vector 30000 30000  -- invalid
                else TVector $ Vector 1 1  -- a step south-east
    , seps = fromEnum _sside
    , stargetD = EM.empty
    , sexplored = ES.empty
    , sbfsD = EM.empty
    , sselected = ES.empty
    , srunning = Nothing
    , sreport
    , shistory
    , sdisplayed = EM.empty
    , sundo = []
    , sdiscoKind = EM.empty
    , sdiscoEffect = EM.empty
    , sfper = EM.empty
    , srandom = R.mkStdGen 42  -- will be set later
    , slastKM = K.escKM
    , slastRecord = ([], [], 0)
    , slastPlay = []
    , slastLost = ES.empty
    , swaitTimes = 0
    , _sleader = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , sisAI
    , smarkVision = False
    , smarkSmell = True
    , smarkSuspect = False
    , scurDiff = difficultyDefault
    , snxtDiff = difficultyDefault
    , sslots = (EM.empty, EM.empty)
    , slastSlot = SlotChar 0 'Z'
    , slastStore = CInv
    , sescAI = EscAINothing
    , sdebugCli = defDebugModeCli
    }

defaultHistory :: Int -> IO History
defaultHistory configHistoryMax = do
  dateTime <- getClockTime
  let curDate = MU.Text $ T.pack $ calendarTimeToString $ toUTCTime dateTime
  let emptyHist = emptyHistory configHistoryMax
  return $! addReport emptyHist timeZero
         $! singletonReport
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
    put sdisplayed
    put sdiscoKind
    put sdiscoEffect
    put (show srandom)
    put _sleader
    put _sside
    put sisAI
    put smarkVision
    put smarkSmell
    put smarkSuspect
    put scurDiff
    put snxtDiff
    put sslots
    put slastSlot
    put slastStore
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
    sdisplayed <- get
    sdiscoKind <- get
    sdiscoEffect <- get
    g <- get
    _sleader <- get
    _sside <- get
    sisAI <- get
    smarkVision <- get
    smarkSmell <- get
    smarkSuspect <- get
    scurDiff <- get
    snxtDiff <- get
    sslots <- get
    slastSlot <- get
    slastStore <- get
    sdebugCli <- get
    let sbfsD = EM.empty
        sfper = EM.empty
        srandom = read g
        slastKM = K.escKM
        slastRecord = ([], [], 0)
        slastPlay = []
        slastLost = ES.empty
        swaitTimes = 0
        squit = False
        sescAI = EscAINothing
    return $! StateClient{..}

instance Binary RunParams where
  put RunParams{..} = do
    put runLeader
    put runMembers
    put runInitial
    put runStopMsg
    put runWaiting
  get = do
    runLeader <- get
    runMembers <- get
    runInitial <- get
    runStopMsg <- get
    runWaiting <- get
    return $! RunParams{..}
