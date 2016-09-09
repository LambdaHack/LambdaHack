{-# LANGUAGE DeriveGeneric #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Client.State
  ( StateClient(..), emptyStateClient
  , AlterLid
  , updateTarget, getTarget, updateLeader, sside
  , BfsAndPath(..), TgtAndPath(..), toggleMarkSuspect
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import GHC.Generics (Generic)
import qualified System.Random as R

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector

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
  { sxhair       :: !Target        -- ^ the common xhair
  , seps         :: !Int           -- ^ a parameter of the aiming digital line
  , stargetD     :: !(EM.EnumMap ActorId TgtAndPath)
                                   -- ^ targets of our actors in the dungeon
  , sexplored    :: !(ES.EnumSet LevelId)
                                   -- ^ the set of fully explored levels
  , sbfsD        :: !(EM.EnumMap ActorId BfsAndPath)
                                   -- ^ pathfinding distances for our actors
                                   --   and paths to their targets, if any
  , sundo        :: ![CmdAtomic]   -- ^ atomic commands performed to date
  , sdiscoKind   :: !DiscoveryKind    -- ^ remembered item discoveries
  , sdiscoAspect :: !DiscoveryAspect  -- ^ remembered aspects of items
  , sactorAspect :: !ActorAspect   -- ^ best known our actor aspect data
  , sfper        :: !PerLid        -- ^ faction perception indexed by levels
  , salter       :: !AlterLid      -- ^ cached alter ability data for positions
  , srandom      :: !R.StdGen      -- ^ current random generator
  , _sleader     :: !(Maybe ActorId)
                                   -- ^ current picked party leader
  , _sside       :: !FactionId     -- ^ faction controlled by the client
  , squit        :: !Bool          -- ^ exit the game loop
  , scurDiff     :: !Int           -- ^ current game difficulty level
  , snxtDiff     :: !Int           -- ^ next game difficulty level
  , sslots       :: !ItemSlots     -- ^ map from slots to items
  , slastSlot    :: !SlotChar      -- ^ last used slot
  , slastStore   :: ![CStore]      -- ^ last used stores
  , smarkSuspect :: !Bool          -- ^ mark suspect features
  , sdebugCli    :: !DebugModeCli  -- ^ client debugging mode
  }
  deriving Show

data BfsAndPath =
    BfsInvalid
  | BfsOnly {bfsArr :: !(PointArray.Array BfsDistance)}
  | BfsAndPath { bfsArr  :: !(PointArray.Array BfsDistance)
               , bfsPath :: !(EM.EnumMap Point AndPath)
               }
  deriving Show

data TgtAndPath = TgtAndPath {tapTgt :: !Target, tapPath :: !AndPath}
  deriving (Show, Generic)

instance Binary TgtAndPath

type AlterLid = EM.EnumMap LevelId (PointArray.Array Word8)

-- | Initial empty game client state.
emptyStateClient :: FactionId -> StateClient
emptyStateClient _sside =
  StateClient
    { sxhair = TVector $ Vector 30000 30000  -- invalid; AI recomputes ASAP
    , seps = fromEnum _sside
    , stargetD = EM.empty
    , sexplored = ES.empty
    , sbfsD = EM.empty
    , sundo = []
    , sdiscoKind = EM.empty
    , sdiscoAspect = EM.empty
    , sactorAspect = EM.empty
    , sfper = EM.empty
    , salter = EM.empty
    , srandom = R.mkStdGen 42  -- will be set later
    , _sleader = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , scurDiff = difficultyDefault
    , snxtDiff = difficultyDefault
    , sslots = ItemSlots EM.empty EM.empty
    , slastSlot = SlotChar 0 'Z'
    , slastStore = []
    , smarkSuspect = False
    , sdebugCli = defDebugModeCli
    }

toggleMarkSuspect :: StateClient -> StateClient
toggleMarkSuspect s@StateClient{smarkSuspect} =
  s {smarkSuspect = not smarkSuspect}

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
                            `twith` (side1, side2, leader, s))
     $ cli {_sleader = Just leader}

sside :: StateClient -> FactionId
sside = _sside

instance Binary StateClient where
  put StateClient{..} = do
    put sxhair
    put seps
    put stargetD
    put sexplored
    put sundo
    put sdiscoKind
    put sdiscoAspect
    put (show srandom)
    put _sleader
    put _sside
    put scurDiff
    put snxtDiff
    put sslots
    put slastSlot
    put slastStore
    put smarkSuspect
    put sdebugCli  -- TODO: this is overwritten at once
  get = do
    sxhair <- get
    seps <- get
    stargetD <- get
    sexplored <- get
    sundo <- get
    sdiscoKind <- get
    sdiscoAspect <- get
    g <- get
    _sleader <- get
    _sside <- get
    scurDiff <- get
    snxtDiff <- get
    sslots <- get
    slastSlot <- get
    slastStore <- get
    smarkSuspect <- get
    sdebugCli <- get
    let sbfsD = EM.empty
        sfper = EM.empty
        sactorAspect = EM.empty
        salter = EM.empty
        srandom = read g
        squit = False
    return $! StateClient{..}
