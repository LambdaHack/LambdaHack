{-# LANGUAGE DeriveGeneric #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Client.State
  ( StateClient(..), emptyStateClient
  , AlterLid
  , updateTarget, getTarget, updateLeader, sside
  , BfsAndPath(..), TgtAndPath(..), cycleMarkSuspect
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)
import qualified System.Random as R

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Content.ModeKind (ModeKind)

-- | Client state, belonging to a single faction.
-- Some of the data, e.g, the history, carries over
-- from game to game, even across playing sessions.
--
-- When many actors want to fling at the same target, they set
-- their personal targets to follow the common xhair.
-- When each wants to kill a fleeing enemy they recently meleed,
-- they keep the enemies as their personal targets.
--
-- Data invariant: if @_sleader@ is @Nothing@ then so is @srunning@.
data StateClient = StateClient
  { seps          :: Int            -- ^ a parameter of the aiming digital line
  , stargetD      :: EM.EnumMap ActorId TgtAndPath
                                    -- ^ targets of our actors in the dungeon
  , sexplored     :: ES.EnumSet LevelId
                                    -- ^ the set of fully explored levels
  , sbfsD         :: EM.EnumMap ActorId BfsAndPath
                                    -- ^ pathfinding distances for our actors
                                    --   and paths to their targets, if any
  , sundo         :: [CmdAtomic]    -- ^ atomic commands performed to date
  , sdiscoBenefit :: DiscoveryBenefit
                                    -- ^ remembered AI benefits of items
  , sfper         :: PerLid         -- ^ faction perception indexed by levels
  , salter        :: AlterLid       -- ^ cached alter ability data for positions
  , srandom       :: R.StdGen       -- ^ current random generator
  , _sleader      :: Maybe ActorId  -- ^ candidate new leader of the faction;
                                    --   Faction._gleader is the old leader
  , _sside        :: FactionId      -- ^ faction controlled by the client
  , squit         :: Bool           -- ^ exit the game loop
  , scurChal      :: Challenge      -- ^ current game challenge setup
  , snxtChal      :: Challenge      -- ^ next game challenge setup
  , snxtScenario  :: Int            -- ^ next game scenario number
  , smarkSuspect  :: Int            -- ^ mark suspect features
  , scondInMelee  :: EM.EnumMap LevelId (Maybe Bool)
                                    -- ^ condInMelee value, unless invalidated
  , svictories    :: EM.EnumMap (Kind.Id ModeKind) (M.Map Challenge Int)
                                    -- ^ won games at particular difficulty lvls
  , soptions      :: ClientOptions  -- ^ client debugging mode
  }
  deriving Show

data BfsAndPath =
    BfsInvalid
  | BfsAndPath { bfsArr  :: PointArray.Array BfsDistance
               , bfsPath :: EM.EnumMap Point AndPath
               }
  deriving Show

data TgtAndPath = TgtAndPath {tapTgt :: Target, tapPath :: AndPath}
  deriving (Show, Generic)

instance Binary TgtAndPath

type AlterLid = EM.EnumMap LevelId (PointArray.Array Word8)

-- | Initial empty game client state.
emptyStateClient :: FactionId -> StateClient
emptyStateClient _sside =
  StateClient
    { seps = fromEnum _sside
    , stargetD = EM.empty
    , sexplored = ES.empty
    , sbfsD = EM.empty
    , sundo = []
    , sdiscoBenefit = EM.empty
    , sfper = EM.empty
    , salter = EM.empty
    , srandom = R.mkStdGen 42  -- will get modified in this and future games
    , _sleader = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , scurChal = defaultChallenge
    , snxtChal = defaultChallenge
    , snxtScenario = 0
    , smarkSuspect = 1
    , scondInMelee = EM.empty
    , svictories = EM.empty
    , soptions = defClientOptions
    }

cycleMarkSuspect :: StateClient -> StateClient
cycleMarkSuspect s@StateClient{smarkSuspect} =
  s {smarkSuspect = (smarkSuspect + 1) `mod` 3}

-- | Update target parameters within client state.
updateTarget :: ActorId -> (Maybe Target -> Maybe Target) -> StateClient
             -> StateClient
updateTarget aid f cli =
  let f2 tp = case f $ fmap tapTgt tp of
        Nothing -> Nothing
        Just tgt -> Just $ TgtAndPath tgt NoPath  -- reset path
  in cli {stargetD = EM.alter f2 aid (stargetD cli)}

-- | Get target parameters from client state.
getTarget :: ActorId -> StateClient -> Maybe Target
getTarget aid cli = fmap tapTgt $ EM.lookup aid $ stargetD cli

-- | Update picked leader within state. Verify actor's faction.
updateLeader :: ActorId -> State -> StateClient -> StateClient
updateLeader leader s cli =
  let side1 = bfid $ getActorBody leader s
      side2 = sside cli
  in assert (side1 == side2 `blame` "enemy actor becomes our leader"
                            `swith` (side1, side2, leader, s))
     $ cli {_sleader = Just leader}

sside :: StateClient -> FactionId
sside = _sside

instance Binary StateClient where
  put StateClient{..} = do
    put seps
    put stargetD
    put sexplored
    put sundo
    put sdiscoBenefit
    put (show srandom)
    put _sleader
    put _sside
    put scurChal
    put snxtChal
    put snxtScenario
    put smarkSuspect
    put scondInMelee
    put svictories
    put soptions
#ifdef WITH_EXPENSIVE_ASSERTIONS
    put sfper
#endif
  get = do
    seps <- get
    stargetD <- get
    sexplored <- get
    sundo <- get
    sdiscoBenefit <- get
    g <- get
    _sleader <- get
    _sside <- get
    scurChal <- get
    snxtChal <- get
    snxtScenario <- get
    smarkSuspect <- get
    scondInMelee <- get
    svictories <- get
    soptions <- get
    let sbfsD = EM.empty
        salter = EM.empty
        srandom = read g
        squit = False
#ifndef WITH_EXPENSIVE_ASSERTIONS
        sfper = EM.empty
#else
    sfper <- get
#endif
    return $! StateClient{..}
