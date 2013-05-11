{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Client.State
  ( StateClient(..), defStateClient, defHistory
  , updateTarget, getTarget, updateLeader, sside
  , TgtMode(..), Target(..)
  , toggleMarkVision, toggleMarkSmell, toggleMarkSuspect
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import Data.Typeable
import Game.LambdaHack.Common.Vector
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Client.Animation
import Game.LambdaHack.Client.Config
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Utils.Assert

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
-- Data invariant: if @_sleader@ is @Nothing@ then so is @srunning@.
data StateClient = StateClient
  { stgtMode     :: !(Maybe TgtMode)  -- ^ targeting mode
  , scursor      :: !(Maybe Point)    -- ^ cursor coordinates
  , seps         :: !Int           -- ^ a parameter of the tgt digital line
  , starget      :: !(EM.EnumMap ActorId Target)
                                -- ^ targets of all our actors in the dungeon
  , srunning     :: !(Maybe (Vector, Int))  -- ^ direction and distance of running
  , sreport      :: !Report        -- ^ current messages
  , shistory     :: !History       -- ^ history of messages
  , sundo        :: ![Atomic]      -- ^ atomic commands performed to date
  , sdisco       :: !Discovery     -- ^ remembered item discoveries
  , sfper        :: !FactionPers   -- ^ faction perception indexed by levels
  , srandom      :: !R.StdGen      -- ^ current random generator
  , sconfigUI    :: !ConfigUI      -- ^ this client config (including initial RNG)
  , slastKey     :: !(Maybe K.KM)  -- ^ last command key pressed
  , sframe       :: ![(Maybe SingleFrame, Bool)]  -- ^ accumulated frames
  , _sleader     :: !(Maybe ActorId)  -- ^ selected actor
  , _sside       :: !FactionId     -- ^ faction controlled by the client
  , squit        :: !Bool          -- ^ exit the game loop
  , sisAI        :: !Bool          -- ^ whether it's an AI client
  , smarkVision  :: !Bool        -- ^ mark leader and party FOV
  , smarkSmell   :: !Bool        -- ^ mark smell, if the leader can smell
  , smarkSuspect :: !Bool        -- ^ mark suspect features
  }
  deriving (Show, Typeable)

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

-- | Initial game client state.
defStateClient :: History -> ConfigUI -> FactionId -> Bool
               -> StateClient
defStateClient shistory sconfigUI _sside sisAI = do
  StateClient
    { stgtMode  = Nothing
    , scursor   = Nothing
    , seps      = 0
    , starget   = EM.empty
    , srunning  = Nothing
    , sreport   = emptyReport
    , shistory
    , sundo     = []
    , sdisco    = EM.empty
    , sfper     = EM.empty
    , sconfigUI
    , srandom = R.mkStdGen 42  -- will be set later
    , slastKey  = Nothing
    , sframe    = []
    , _sleader  = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , sisAI
    , smarkVision = False
    , smarkSmell  = False
    , smarkSuspect = False
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
updateTarget aid f cli = cli { starget = EM.alter f aid (starget cli) }

-- | Get target parameters from client state.
getTarget :: ActorId -> StateClient -> Maybe Target
getTarget aid cli = EM.lookup aid (starget cli)

-- | Update selected actor within state. Verify actor's faction.
updateLeader :: ActorId -> State -> StateClient -> StateClient
updateLeader leader s cli =
  let side1 = bfaction $ getActorBody leader s
      side2 = sside cli
  in assert (side1 == side2 `blame` (side1, side2, leader, s))
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
    put starget
    put srunning
    put shistory
    put sundo
    put sdisco
    put (show srandom)
    put sconfigUI
    put sframe
    put _sleader
    put _sside
    put sisAI
    put smarkVision
    put smarkSmell
    put smarkSuspect
  get = do
    stgtMode <- get
    scursor <- get
    seps <- get
    starget <- get
    srunning <- get
    shistory <- get
    sundo <- get
    sdisco <- get
    g <- get
    sconfigUI <- get
    sframe <- get
    _sleader <- get
    _sside <- get
    sisAI <- get
    smarkVision <- get
    smarkSmell <- get
    smarkSuspect <- get
    let sreport = emptyReport
        sfper = EM.empty
        srandom = read g
        slastKey = Nothing
        squit = False
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
