{-# LANGUAGE TupleSections #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), ActorTime, ActorPushedBy
  , emptyStateServer, updateActorTime, lookupActorTime, ageActor
#ifdef EXPOSE_INTERNAL
  , GearOfTeams
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified System.Random.SplitMix32 as SM

import Game.LambdaHack.Common.Analytics
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Types
import Game.LambdaHack.Content.FactionKind (TeamContinuity)
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.ServerOptions

-- | State with server-specific data, including a copy of each client's
-- basic game state, but not the server's basic state.
data StateServer = StateServer
  { sactorTime    :: ActorTime      -- ^ absolute times of actors next actions
  , strajTime     :: ActorTime      -- ^ and same for actors with trajectories
  , strajPushedBy :: ActorPushedBy  -- ^ culprits for actors with trajectories
  , steamGear     :: GearOfTeams    -- ^ metagame persistent personal
                                    --   characteristics and favourite gear
                                    --   of each numbered continued team member
  , steamGearCur  :: GearOfTeams    -- ^ gear preferences to be taken into
                                    --   account in the current game
  , stcounter     :: EM.EnumMap TeamContinuity Int
                                    -- ^ stores next continued team character
                                    --   identity index number in this game
  , sfactionAn    :: FactionAnalytics
                                    -- ^ various past events data for factions
  , sactorAn      :: ActorAnalytics -- ^ various past events data for actors
  , sgenerationAn :: GenerationAnalytics
                                    -- ^ item creation statistics, by item lore
  , sactorStasis  :: ES.EnumSet ActorId
                                    -- ^ actors currently in time stasis,
                                    --   invulnerable to time warps until move
  , sdiscoKindRev :: DiscoveryKindRev
                                    -- ^ reverse map, used for item creation
  , suniqueSet    :: UniqueSet      -- ^ already generated unique items
  , sitemRev      :: ItemRev        -- ^ reverse id map, used for item creation
  , sflavour      :: FlavourMap     -- ^ association of flavour to item kinds
  , sacounter     :: ActorId        -- ^ stores next actor index
  , sicounter     :: ItemId         -- ^ stores next item index
  , snumSpawned   :: EM.EnumMap LevelId Int
                                    -- ^ how many spawned so far on the level
  , sbandSpawned  :: IM.IntMap Int  -- ^ how many times such group spawned
  , sundo         :: () -- [CmdAtomic] -- ^ atomic commands performed to date
  , sclientStates :: EM.EnumMap FactionId State
                                    -- ^ each faction state, as seen by clients
  , smetaBackup   :: EM.EnumMap TeamContinuity DiscoveryKind
                                    -- ^ discovery info for absent factions
  , sperFid       :: PerFid         -- ^ perception of all factions
  , sperValidFid  :: PerValidFid    -- ^ perception validity for all factions
  , sperCacheFid  :: PerCacheFid    -- ^ perception cache of all factions
  , sfovLucidLid  :: FovLucidLid    -- ^ ambient or shining light positions
  , sfovClearLid  :: FovClearLid    -- ^ clear tiles positions
  , sfovLitLid    :: FovLitLid      -- ^ ambient light positions
  , sarenas       :: ES.EnumSet LevelId
                                    -- ^ the set of active arenas
  , svalidArenas  :: Bool           -- ^ whether active arenas valid
  , srandom       :: SM.SMGen       -- ^ current random generator
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

-- | Per-team, per-actor metagame persistent favourite organs and gear.
type GearOfTeams = EM.EnumMap
                     TeamContinuity
                     (IM.IntMap [(GroupName ItemKind, ContentId ItemKind)])

-- | Initial, empty game server state.
emptyStateServer :: StateServer
emptyStateServer =
  StateServer
    { sactorTime = EM.empty
    , strajTime = EM.empty
    , strajPushedBy = EM.empty
    , steamGear = EM.empty
    , steamGearCur = EM.empty
    , stcounter = EM.empty
    , sfactionAn = EM.empty
    , sactorAn = EM.empty
    , sgenerationAn = EM.fromDistinctAscList
                      $ map (, EM.empty) [minBound .. maxBound]
    , sactorStasis = ES.empty
    , sdiscoKindRev = emptyDiscoveryKindRev
    , suniqueSet = ES.empty
    , sitemRev = HM.empty
    , sflavour = emptyFlavourMap
    , sacounter = toEnum 0
    , sicounter = toEnum 0
    , snumSpawned = EM.empty
    , sbandSpawned = IM.fromList [(1, 0), (2, 0), (3, 0)]
    , sundo = ()
    , sclientStates = EM.empty
    , smetaBackup = EM.empty
    , sperFid = EM.empty
    , sperValidFid = EM.empty
    , sperCacheFid = EM.empty
    , sfovLucidLid = EM.empty
    , sfovClearLid = EM.empty
    , sfovLitLid = EM.empty
    , sarenas = ES.empty
    , svalidArenas = False
    , srandom = SM.mkSMGen 42
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
  EM.lookup aid m2

ageActor :: FactionId -> LevelId -> ActorId -> Delta Time -> ActorTime
         -> ActorTime
ageActor !fid !lid !aid !delta =
  EM.adjust (EM.adjust (EM.adjust (`timeShift` delta) aid) lid) fid

instance Binary StateServer where
  put StateServer{..} = do
    put sactorTime
    put strajTime
    put strajPushedBy
    put steamGear
    put steamGearCur
    put stcounter
    put sfactionAn
    put sactorAn
    put sgenerationAn
    put sactorStasis
    put sdiscoKindRev
    put suniqueSet
    put sitemRev
    put sflavour
    put sacounter
    put sicounter
    put snumSpawned
    put sbandSpawned
    put sclientStates
    put smetaBackup
    put (show srandom)
    put srngs
    put soptions
  get = do
    sactorTime <- get
    strajTime <- get
    strajPushedBy <- get
    steamGear <- get
    steamGearCur <- get
    stcounter <- get
    sfactionAn <- get
    sactorAn <- get
    sgenerationAn <- get
    sactorStasis <- get
    sdiscoKindRev <- get
    suniqueSet <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    snumSpawned <- get
    sbandSpawned <- get
    sclientStates <- get
    smetaBackup <- get
    g <- get
    srngs <- get
    soptions <- get
    let srandom = read g
        sundo = ()
        sperFid = EM.empty
        sperValidFid = EM.empty
        sperCacheFid = EM.empty
        sfovLucidLid = EM.empty
        sfovClearLid = EM.empty
        sfovLitLid = EM.empty
        sarenas = ES.empty
        svalidArenas = False
        sbreakLoop = False
        sbreakASAP = False
        swriteSave = False
        soptionsNxt = defServerOptions
    return $! StateServer{..}
