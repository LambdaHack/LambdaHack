-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), emptyStateServer
  , RNGs(..)
  , ActorTime, updateActorTime, ageActor
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

-- | Global, server state.
data StateServer = StateServer
  { sactorTime    :: ActorTime         -- ^ absolute times of next actions
  , sdiscoKindRev :: DiscoveryKindRev  -- ^ reverse map, used for item creation
  , suniqueSet    :: UniqueSet         -- ^ already generated unique items
  , sitemSeedD    :: ItemSeedDict  -- ^ map from item ids to item seeds
  , sitemRev      :: ItemRev       -- ^ reverse id map, used for item creation
  , sflavour      :: FlavourMap    -- ^ association of flavour to items
  , sacounter     :: ActorId       -- ^ stores next actor index
  , sicounter     :: ItemId        -- ^ stores next item index
  , snumSpawned   :: EM.EnumMap LevelId Int
  , sundo         :: [CmdAtomic]   -- ^ atomic commands performed to date
  , sclientStates :: EM.EnumMap FactionId State
                                   -- ^ each faction state, as seen by clients
  , sperFid       :: PerFid        -- ^ perception of all factions
  , sperValidFid  :: PerValidFid   -- ^ perception validity for all factions
  , sperCacheFid  :: PerCacheFid   -- ^ perception cache of all factions
  , sfovLucidLid  :: FovLucidLid   -- ^ ambient or shining light positions
  , sfovClearLid  :: FovClearLid   -- ^ clear tiles positions
  , sfovLitLid    :: FovLitLid     -- ^ ambient light positions
  , sarenas       :: [LevelId]     -- ^ active arenas
  , svalidArenas  :: Bool          -- ^ whether active arenas valid
  , srandom       :: R.StdGen      -- ^ current random generator
  , srngs         :: RNGs          -- ^ initial random generators
  , squit         :: Bool          -- ^ exit the game loop
  , swriteSave    :: Bool          -- ^ write savegame to a file now
  , sdebugSer     :: DebugModeSer  -- ^ current debugging mode
  , sdebugNxt     :: DebugModeSer  -- ^ debugging mode for the next game
  }
  deriving (Show)

type ActorTime =
  EM.EnumMap FactionId (EM.EnumMap LevelId (EM.EnumMap ActorId Time))

updateActorTime :: FactionId -> LevelId -> ActorId -> Time -> ActorTime
                -> ActorTime
updateActorTime !fid !lid !aid !time =
  EM.adjust (EM.adjust (EM.insert aid time) lid) fid

ageActor :: FactionId -> LevelId -> ActorId -> Delta Time -> ActorTime
         -> ActorTime
ageActor !fid !lid !aid !delta =
  EM.adjust (EM.adjust (EM.adjust (`timeShift` delta) aid) lid) fid

-- | Initial, empty game server state.
emptyStateServer :: StateServer
emptyStateServer =
  StateServer
    { sactorTime = EM.empty
    , sdiscoKindRev = EM.empty
    , suniqueSet = ES.empty
    , sitemSeedD = EM.empty
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
    , squit = False
    , swriteSave = False
    , sdebugSer = defDebugModeSer
    , sdebugNxt = defDebugModeSer
    }

instance Binary StateServer where
  put StateServer{..} = do
    put sactorTime
    put sdiscoKindRev
    put suniqueSet
    put sitemSeedD
    put sitemRev
    put sflavour
    put sacounter
    put sicounter
    put snumSpawned
    put sclientStates
    put (show srandom)
    put srngs
    put sdebugSer
  get = do
    sactorTime <- get
    sdiscoKindRev <- get
    suniqueSet <- get
    sitemSeedD <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    snumSpawned <- get
    sclientStates <- get
    g <- get
    srngs <- get
    sdebugSer <- get
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
        squit = False
        swriteSave = False
        sdebugNxt = defDebugModeSer
    return $! StateServer{..}
