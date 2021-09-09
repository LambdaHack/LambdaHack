{-# LANGUAGE DeriveGeneric #-}
-- | Client-specific game state components.
module Game.LambdaHack.Client.State
  ( StateClient(..), AlterLid, BfsAndPath(..)
  , TgtAndPath(..), Target(..), TGoal(..)
  , emptyStateClient, cycleMarkSuspect
  , updateTarget, getTarget, updateLeader, sside, sleader
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Primitive.PrimArray as PA
import           GHC.Generics (Generic)
import qualified System.Random.SplitMix32 as SM

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ModeKind (ModeKind)
import           Game.LambdaHack.Definition.Defs

-- | Client state, belonging to a single faction.
data StateClient = StateClient
  { seps          :: Int            -- ^ a parameter of the aiming digital line
  , stargetD      :: EM.EnumMap ActorId TgtAndPath
      -- ^ targets of our actors in the dungeon; this is only useful for AI
      --   and for directing non-pointmen, in particular with following
      --   doctrines, where non-pointmen go to the pointman's target
  , sfleeD        :: EM.EnumMap ActorId (Point, Time)
                                    -- ^ the position and time of last fleeing
                                    --   attempt (regardless if succeeded)
  , sexplored     :: ES.EnumSet LevelId
                                    -- ^ the set of fully explored levels
  , sbfsD         :: EM.EnumMap ActorId BfsAndPath
                                    -- ^ pathfinding data for our actors
  , sundo         :: () -- [CmdAtomic] -- ^ atomic commands performed to date
  , sdiscoBenefit :: DiscoveryBenefit
      -- ^ remembered AI benefits of items; could be recomputed at resume,
      --   but they are costly to generate and not too large
  , sfper         :: PerLid         -- ^ faction perception indexed by level
  , salter        :: AlterLid       -- ^ cached alter skill data for positions
                                    --   (actually, @Tile.alterMinWalk@ instead)
  , srandom       :: SM.SMGen       -- ^ current random generator
  , _sleader      :: Maybe ActorId  -- ^ candidate new leader of the faction;
                                    --   Faction.gleader is the old leader
  , _sside        :: FactionId      -- ^ faction controlled by the client
  , squit         :: Bool           -- ^ exit the game loop
  , scurChal      :: Challenge      -- ^ current game challenge setup
  , snxtChal      :: Challenge      -- ^ next game challenge setup
  , smarkSuspect  :: Int            -- ^ whether to mark suspect features
  , scondInMelee  :: ES.EnumSet LevelId
                                    -- ^ whether we are in melee, per level
  , svictories    :: EM.EnumMap (ContentId ModeKind) (M.Map Challenge Int)
                                    -- ^ won games at particular difficulty lvls
  , scampings     :: ES.EnumSet (ContentId ModeKind)  -- ^ camped games
  , srestarts     :: ES.EnumSet (ContentId ModeKind)  -- ^ restarted games
  , soptions      :: ClientOptions  -- ^ client options
  , stabs         :: (PA.PrimArray PointI, PA.PrimArray PointI)
      -- ^ Instead of a BFS queue (list) we use these two arrays,
      --   for (JS) speed. They need to be per-client distinct,
      --   because sometimes multiple clients interleave BFS computation.
  }
  deriving Show

type AlterLid = EM.EnumMap LevelId (PointArray.Array Word8)

-- | Pathfinding distances to all reachable positions of an actor
-- and a shortest paths to some of the positions.
data BfsAndPath =
    BfsInvalid
  | BfsAndPath (PointArray.Array BfsDistance)
               (EM.EnumMap Point AndPath)
  deriving Show

-- | Actor's target and a path to it, if any.
data TgtAndPath = TgtAndPath {tapTgt :: Target, tapPath :: Maybe AndPath}
  deriving (Show, Generic)

instance Binary TgtAndPath

-- | The type of na actor target.
data Target =
    TEnemy ActorId              -- ^ target an enemy
  | TNonEnemy ActorId           -- ^ target a friend or neutral
  | TPoint TGoal LevelId Point  -- ^ target a concrete spot
  | TVector Vector              -- ^ target position relative to actor
  deriving (Show, Eq, Generic)

instance Binary Target

-- | The goal of an actor.
data TGoal =
    TStash FactionId  -- ^ shared inventory stash of our or an enemy faction
  | TEnemyPos ActorId  -- ^ last seen position of the targeted actor
  | TEmbed ItemBag Point  -- ^ embedded item that can be triggered;
                          -- in @TPoint (TEmbed bag p) _ q@ usually @bag@ is
                          -- embbedded in @p@ and @q@ is an adjacent open tile
  | TItem ItemBag  -- ^ item lying on the ground
  | TSmell  -- ^ smell potentially left by enemies
  | TBlock  -- ^ a blocking tile to be approached (and, e.g., revealed
            --   to be walkable or altered or searched)
  | TUnknown  -- ^ an unknown tile to be explored
  | TKnown  -- ^ a known tile to be patrolled
  | THideout  -- ^ a hideout to either flee to or find a hidden enemy sniper in
  deriving (Show, Eq, Generic)

instance Binary TGoal

-- | Initial empty game client state.
emptyStateClient :: FactionId -> StateClient
emptyStateClient _sside =
  StateClient
    { seps = fromEnum _sside
    , stargetD = EM.empty
    , sfleeD = EM.empty
    , sexplored = ES.empty
    , sbfsD = EM.empty
    , sundo = ()
    , sdiscoBenefit = EM.empty
    , sfper = EM.empty
    , salter = EM.empty
    , srandom = SM.mkSMGen 42  -- will get modified in this and future games
    , _sleader = Nothing  -- no heroes yet alive
    , _sside
    , squit = False
    , scurChal = defaultChallenge
    , snxtChal = defaultChallenge
    , smarkSuspect = 1
    , scondInMelee = ES.empty
    , svictories = EM.empty
    , scampings = ES.empty
    , srestarts = ES.empty
    , soptions = defClientOptions
    , stabs = (undefined, undefined)
    }

-- | Cycle the 'smarkSuspect' setting.
cycleMarkSuspect :: StateClient -> StateClient
cycleMarkSuspect s@StateClient{smarkSuspect} =
  s {smarkSuspect = (smarkSuspect + 1) `mod` 3}

-- | Update target parameters within client state.
updateTarget :: ActorId -> (Maybe Target -> Maybe Target) -> StateClient
             -> StateClient
updateTarget aid f cli =
  let f2 tp = case f $ fmap tapTgt tp of
        Nothing -> Nothing
        Just tgt -> Just $ TgtAndPath tgt Nothing  -- reset path
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

sleader :: StateClient -> Maybe ActorId
sleader = _sleader

instance Binary StateClient where
  put StateClient{..} = do
    put seps
    put stargetD
    put sfleeD
    put sexplored
    put sdiscoBenefit
    put (show srandom)
    put _sleader
    put _sside
    put scurChal
    put snxtChal
    put smarkSuspect
    put scondInMelee
    put svictories
    put scampings
    put srestarts
    put soptions
#ifdef WITH_EXPENSIVE_ASSERTIONS
    put sfper
#endif
  get = do
    seps <- get
    stargetD <- get
    sfleeD <- get
    sexplored <- get
    sdiscoBenefit <- get
    g <- get
    _sleader <- get
    _sside <- get
    scurChal <- get
    snxtChal <- get
    smarkSuspect <- get
    scondInMelee <- get
    svictories <- get
    scampings <- get
    srestarts <- get
    soptions <- get
    let sbfsD = EM.empty
        sundo = ()
        salter = EM.empty
        srandom = read g
        squit = False
        stabs = (undefined, undefined)
#ifndef WITH_EXPENSIVE_ASSERTIONS
        sfper = EM.empty
#else
    sfper <- get
#endif
    return $! StateClient{..}
