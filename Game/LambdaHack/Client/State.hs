{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Client.State
  ( StateClient(..), defStateClient, defHistory
  , updateTarget, getTarget
  , invalidateSelectedLeader, updateSelectedLeader, getLeader, targetToPos
  , TgtMode(..), Target(..)
  , DebugModeCli(..), toggleMarkVision, toggleMarkSmell, toggleOmniscient
  ) where

import Control.Monad
import Data.Binary
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable
import Game.LambdaHack.Vector
import qualified NLP.Miniutter.English as MU
import System.Time

import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Animation
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
-- Data invariant: if @_sleader@ is @Nothing@ then so is @srunning@.
data StateClient = StateClient
  { stgtMode  :: !(Maybe TgtMode)  -- ^ targeting mode
  , scursor   :: !(Maybe Point)    -- ^ cursor coordinates
  , seps      :: !Int           -- ^ a parameter of the tgt digital line
  , starget   :: !(IM.IntMap Target)  -- ^ targets of all actors in the dungeon
  , srunning  :: !(Maybe (Vector, Int))  -- ^ direction and distance of running
  , sreport   :: !Report        -- ^ current messages
  , shistory  :: !History       -- ^ history of messages
  , sper      :: !FactionPers   -- ^ faction perception indexed by levels
  , slastKey  :: !(Maybe K.KM)  -- ^ last command key pressed
  , sframe    :: ![(Maybe SingleFrame, Bool)]  -- ^ accumulated frames
  , _sleader  :: !(Maybe ActorId)  -- ^ selected actor
  , sdebugCli :: !DebugModeCli  -- ^ debugging mode
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

data DebugModeCli = DebugModeCli
  { smarkVision :: !Bool
  , smarkSmell  :: !Bool
  , somniscient :: !Bool
  }
  deriving Show

-- | Initial game client state.
defStateClient :: History -> StateClient
defStateClient shistory = do
  StateClient
    { stgtMode  = Nothing
    , scursor   = Nothing
    , seps      = 0
    , starget   = IM.empty
    , srunning  = Nothing
    , sreport   = emptyReport
    , shistory
    , sper      = M.empty
    , slastKey  = Nothing
    , sframe    = []
    , _sleader  = Nothing  -- no heroes yet alive
    , sdebugCli = defDebugModeCli
    }

defDebugModeCli :: DebugModeCli
defDebugModeCli = DebugModeCli
  { smarkVision = False
  , smarkSmell  = False
  , somniscient = False
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
updateTarget aid f cli = cli { starget = IM.alter f aid (starget cli) }

-- | Get target parameters from client state.
getTarget :: ActorId -> StateClient -> Maybe Target
getTarget aid cli = IM.lookup aid (starget cli)

-- | Invalidate selected actor, e.g., to avoid violatng the invariant.
-- If the the leader was running, stop the run.
invalidateSelectedLeader :: StateClient -> StateClient
invalidateSelectedLeader cli = cli {srunning = Nothing, _sleader = Nothing}

-- | Update selected actor within state. The actor is required
-- to belong to the selected level and selected faction.
updateSelectedLeader :: ActorId -> State -> StateClient -> StateClient
updateSelectedLeader leader s cli =
  let la = lactor $ sdungeon s M.! sarena s
      side1 = fmap bfaction $ IM.lookup leader la
      side2 = Just $ sside s
  in assert (side1 == side2 `blame` (side1, side2, leader, sarena s, s))
     $ cli {_sleader = Just leader}

getLeader :: StateClient -> Maybe ActorId
getLeader = _sleader

-- | Calculate the position of leader's target.
targetToPos :: StateClient -> State -> Maybe Point
targetToPos cli@StateClient{scursor} s = do
  leader <- getLeader cli
  case getTarget leader cli of
    Just (TPos pos) -> return pos
    Just (TEnemy a _ll) -> do
      guard $ memActor a s           -- alive and visible?
      return $! bpos (getActorBody a s)
    Nothing -> scursor

toggleMarkVision :: StateClient -> StateClient
toggleMarkVision s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkVision}} =
  s {sdebugCli = sdebugCli {smarkVision = not smarkVision}}

toggleMarkSmell :: StateClient -> StateClient
toggleMarkSmell s@StateClient{sdebugCli=sdebugCli@DebugModeCli{smarkSmell}} =
  s {sdebugCli = sdebugCli {smarkSmell = not smarkSmell}}

toggleOmniscient :: StateClient -> StateClient
toggleOmniscient s@StateClient{sdebugCli=sdebugCli@DebugModeCli{somniscient}} =
  s {sdebugCli = sdebugCli {somniscient = not somniscient}}

instance Binary StateClient where
  put StateClient{..} = assert (null sframe `blame` sframe) $ do
    put stgtMode
    put scursor
    put seps
    put starget
    put srunning
    put sreport
    put shistory
    put _sleader
  get = do
    stgtMode <- get
    scursor <- get
    seps <- get
    starget <- get
    srunning <- get
    sreport <- get
    shistory <- get
    _sleader <- get
    let sper = M.empty
        slastKey = Nothing
        sframe = []
        sdebugCli = defDebugModeCli
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
