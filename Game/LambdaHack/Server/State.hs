{-# LANGUAGE DeriveGeneric #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..)
  , ActorTime, ActorPushedBy
  , ActorAnalytics, KillMap, Analytics(..), KillHow(..)
  , addKill, emptyStateServer, updateActorTime, lookupActorTime, ageActor
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import           GHC.Generics (Generic)
import qualified System.Random as R

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.ServerOptions

-- | State with server-specific data, including a copy of each client's
-- basic game state, but not the server's basic state.
data StateServer = StateServer
  { sactorTime    :: ActorTime      -- ^ absolute times of actors next actions
  , strajTime     :: ActorTime      -- ^ and same for actors with trajectories
  , strajPushedBy :: ActorPushedBy  -- ^ culprits for actors with trajectories
  , sanalytics    :: ActorAnalytics -- ^ various past events data for actors
  , sactorStasis  :: ES.EnumSet ActorId
                                    -- ^ actors currently in time stasis,
                                    --   invulnerable to time warps until move
  , sdiscoKindRev :: DiscoveryKindRev
                                    -- ^ reverse map, used for item creation
  , suniqueSet    :: UniqueSet      -- ^ already generated unique items
  , sitemRev      :: ItemRev        -- ^ reverse id map, used for item creation
  , sflavour      :: FlavourMap     -- ^ association of flavour to items
  , sacounter     :: ActorId        -- ^ stores next actor index
  , sicounter     :: ItemId         -- ^ stores next item index
  , snumSpawned   :: EM.EnumMap LevelId Int
  , sundo         :: [CmdAtomic]    -- ^ atomic commands performed to date
  , sclientStates :: EM.EnumMap FactionId State
                                    -- ^ each faction state, as seen by clients
  , sperFid       :: PerFid         -- ^ perception of all factions
  , sperValidFid  :: PerValidFid    -- ^ perception validity for all factions
  , sperCacheFid  :: PerCacheFid    -- ^ perception cache of all factions
  , sfovLucidLid  :: FovLucidLid    -- ^ ambient or shining light positions
  , sfovClearLid  :: FovClearLid    -- ^ clear tiles positions
  , sfovLitLid    :: FovLitLid      -- ^ ambient light positions
  , sarenas       :: [LevelId]      -- ^ active arenas
  , svalidArenas  :: Bool           -- ^ whether active arenas valid
  , srandom       :: R.StdGen       -- ^ current random generator
  , srngs         :: RNGs           -- ^ initial random generators
  , sbreakLoop    :: Bool           -- ^ exit game loop after clip's end;
                                    --   usually no game save follows
  , sbreakASAP    :: Bool           -- ^ exit game loop ASAP; usually with save
  , swriteSave    :: Bool           -- ^ write savegame to file after loop exit
  , soptions      :: ServerOptions  -- ^ current commandline options
  , soptionsNxt   :: ServerOptions  -- ^ options for the next game
  }
  deriving Show

-- | Position in time for each actor, grouped by level and by faction.
type ActorTime =
  EM.EnumMap FactionId (EM.EnumMap LevelId (EM.EnumMap ActorId Time))

-- | Record who last propelled a given actor with trajectory.
type ActorPushedBy = EM.EnumMap ActorId ActorId

-- | Analytics data for each live actor.
type ActorAnalytics = EM.EnumMap ActorId Analytics

type KillMap = EM.EnumMap FactionId (EM.EnumMap ItemId Int)

-- | Statistics of past events concerning an actor.
data Analytics = Analytics
  { akillMelee  :: KillMap
  , akillRanged :: KillMap
  , akillBlast  :: KillMap
  , akillPush   :: KillMap
  , akillTile   :: KillMap
  , akillCatch  :: KillMap
  , akillLaunch :: KillMap  -- registered only when they hit an actor
  }
  deriving (Show, Generic)

instance Binary Analytics

data KillHow =
    KillMelee
  | KillRanged
  | KillBlast
  | KillPush
  | KillTile
  | KillCatch
  | KillLaunch

emptyAnalytics :: Analytics
emptyAnalytics = Analytics
  { akillMelee  = EM.empty
  , akillRanged = EM.empty
  , akillBlast  = EM.empty
  , akillPush   = EM.empty
  , akillTile   = EM.empty
  , akillCatch  = EM.empty
  , akillLaunch = EM.empty
  }

alterIncrement :: FactionId -> ItemId -> KillMap -> KillMap
{-# NOINLINE alterIncrement #-}
alterIncrement fid iid =
  let f Nothing = Just $ EM.singleton iid 1
      f (Just iidMap) = Just $ EM.alter g iid iidMap
      g Nothing = Just 1
      g (Just n) = Just $ n + 1
  in EM.alter f fid

addKill :: KillHow -> ActorId -> FactionId -> ItemId
        -> ActorAnalytics
        -> ActorAnalytics
{-# INLINE addKill #-}
addKill killHow aid fid iid =
  let applyAtKill an = case killHow of
        KillMelee -> an {akillMelee = alterIncrement fid iid $ akillMelee an}
        KillRanged -> an {akillRanged = alterIncrement fid iid $ akillRanged an}
        KillBlast -> an {akillBlast = alterIncrement fid iid $ akillBlast an}
        KillPush -> an {akillPush = alterIncrement fid iid $ akillPush an}
        KillTile -> an {akillTile = alterIncrement fid iid $ akillTile an}
        KillCatch -> an {akillCatch = alterIncrement fid iid $ akillCatch an}
        KillLaunch -> an {akillLaunch = alterIncrement fid iid $ akillLaunch an}
      f Nothing = Just $ applyAtKill emptyAnalytics
      f (Just an) = Just $ applyAtKill an
  in EM.alter f aid

-- | Initial, empty game server state.
emptyStateServer :: StateServer
emptyStateServer =
  StateServer
    { sactorTime = EM.empty
    , strajTime = EM.empty
    , strajPushedBy = EM.empty
    , sanalytics = EM.empty
    , sactorStasis = ES.empty
    , sdiscoKindRev = emptyDiscoveryKindRev
    , suniqueSet = ES.empty
    , sitemRev = HM.empty
    , sflavour = emptyFlavourMap
    , sacounter = toEnum 0
    , sicounter = toEnum 0
    , snumSpawned = EM.empty
    , sundo = []
    , sclientStates = EM.empty
    , sperFid = EM.empty
    , sperValidFid = EM.empty
    , sperCacheFid = EM.empty
    , sfovLucidLid = EM.empty
    , sfovClearLid = EM.empty
    , sfovLitLid = EM.empty
    , sarenas = []
    , svalidArenas = False
    , srandom = R.mkStdGen 42
    , srngs = RNGs { dungeonRandomGenerator = Nothing
                   , startingRandomGenerator = Nothing }
    , sbreakLoop = False
    , sbreakASAP = False
    , swriteSave = False
    , soptions = defServerOptions
    , soptionsNxt = defServerOptions
    }

updateActorTime :: FactionId -> LevelId -> ActorId -> Time -> ActorTime
                -> ActorTime
updateActorTime !fid !lid !aid !time =
  EM.adjust (EM.adjust (EM.insert aid time) lid) fid

lookupActorTime :: FactionId -> LevelId -> ActorId -> ActorTime
                -> Maybe Time
lookupActorTime !fid !lid !aid !atime = do
  m1 <- EM.lookup fid atime
  m2 <- EM.lookup lid m1
  m3 <- EM.lookup aid m2
  return m3

ageActor :: FactionId -> LevelId -> ActorId -> Delta Time -> ActorTime
         -> ActorTime
ageActor !fid !lid !aid !delta =
  EM.adjust (EM.adjust (EM.adjust (`timeShift` delta) aid) lid) fid

instance Binary StateServer where
  put StateServer{..} = do
    put sactorTime
    put strajTime
    put strajPushedBy
    put sanalytics
    put sactorStasis
    put sdiscoKindRev
    put suniqueSet
    put sitemRev
    put sflavour
    put sacounter
    put sicounter
    put snumSpawned
    put sclientStates
    put (show srandom)
    put srngs
    put soptions
  get = do
    sactorTime <- get
    strajTime <- get
    strajPushedBy <- get
    sanalytics <- get
    sactorStasis <- get
    sdiscoKindRev <- get
    suniqueSet <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    snumSpawned <- get
    sclientStates <- get
    g <- get
    srngs <- get
    soptions <- get
    let srandom = read g
        sundo = []
        sperFid = EM.empty
        sperValidFid = EM.empty
        sperCacheFid = EM.empty
        sfovLucidLid = EM.empty
        sfovClearLid = EM.empty
        sfovLitLid = EM.empty
        sarenas = []
        svalidArenas = False
        sbreakLoop = False
        sbreakASAP = False
        swriteSave = False
        soptionsNxt = defServerOptions
    return $! StateServer{..}
