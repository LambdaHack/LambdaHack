{-# LANGUAGE OverloadedStrings #-}
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
import Game.LambdaHack.Common.Vector
import qualified NLP.Miniutter.English as MU
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Client.Config
import qualified Game.LambdaHack.Common.Key as K
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
  , seps         :: !Int              -- ^ a parameter of the tgt digital line
  , stargetD     :: !(EM.EnumMap ActorId Target)
                                   -- ^ targets of our actors in the dungeon
  , srunning     :: !(Maybe (Vector, Int))
                                   -- ^ direction and distance of running
  , sreport      :: !Report        -- ^ current messages
  , shistory     :: !History       -- ^ history of messages
  , sundo        :: ![Atomic]      -- ^ atomic commands performed to date
  , sdisco       :: !Discovery     -- ^ remembered item discoveries
  , sfper        :: !FactionPers   -- ^ faction perception indexed by levels
  , srandom      :: !R.StdGen      -- ^ current random generator
  , sconfigUI    :: !ConfigUI      -- ^ client config (including initial RNG)
  , slastKey     :: !(Maybe K.KM)  -- ^ last command key pressed
  , _sleader     :: !(Maybe ActorId)  -- ^ selected actor
  , _sside       :: !FactionId     -- ^ faction controlled by the client
  , squit        :: !Bool          -- ^ exit the game loop
  , sisAI        :: !Bool          -- ^ whether it's an AI client
  , smarkVision  :: !Bool          -- ^ mark leader and party FOV
  , smarkSmell   :: !Bool          -- ^ mark smell, if the leader can smell
  , smarkSuspect :: !Bool          -- ^ mark suspect features
  , sdebugCli    :: !Bool          -- ^ show client debug messages
  }
  deriving (Show)

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
    { stgtMode = Nothing
    , scursor = Nothing
    , seps = 0
    , stargetD = EM.empty
    , srunning = Nothing
    , sreport = emptyReport
    , shistory
    , sundo = []
    , sdisco = EM.empty
    , sfper = EM.empty
    , sconfigUI
    , srandom = R.mkStdGen 42  -- will be set later
    , slastKey = Nothing
    , _sleader = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , sisAI
    , smarkVision = False
    , smarkSmell = False
    , smarkSuspect = False
    , sdebugCli = False
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
updateTarget aid f cli = cli { stargetD = EM.alter f aid (stargetD cli) }

-- | Get target parameters from client state.
getTarget :: ActorId -> StateClient -> Maybe Target
getTarget aid cli = EM.lookup aid (stargetD cli)

-- | Update selected actor within state. Verify actor's faction.
updateLeader :: ActorId -> State -> StateClient -> StateClient
updateLeader leader s cli =
  let side1 = bfid $ getActorBody leader s
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
    put stargetD
    put srunning
    put sreport
    put shistory
    put sundo
    put sdisco
    put (show srandom)
    put sconfigUI
    put _sleader
    put _sside
    put sisAI
    put smarkVision
    put smarkSmell
    put smarkSuspect
    put sdebugCli
  get = do
    stgtMode <- get
    scursor <- get
    seps <- get
    stargetD <- get
    srunning <- get
    sreport <- get
    shistory <- get
    sundo <- get
    sdisco <- get
    g <- get
    sconfigUI <- get
    _sleader <- get
    _sside <- get
    sisAI <- get
    smarkVision <- get
    smarkSmell <- get
    smarkSuspect <- get
    sdebugCli <- get
    let sfper = EM.empty
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
