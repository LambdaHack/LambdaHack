-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), ActorTime
  , emptyStateServer, updateActorTime, ageActor
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
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
  { sactorTime    :: ActorTime      -- ^ absolute times of next actions
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

-- | Initial, empty game server state.
emptyStateServer :: StateServer
emptyStateServer =
  StateServer
    { sactorTime = EM.empty
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

ageActor :: FactionId -> LevelId -> ActorId -> Delta Time -> ActorTime
         -> ActorTime
ageActor !fid !lid !aid !delta =
  EM.adjust (EM.adjust (EM.adjust (`timeShift` delta) aid) lid) fid

instance Binary StateServer where
  put StateServer{..} = do
    put sactorTime
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
