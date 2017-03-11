-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), emptyStateServer
  , DebugModeSer(..), defDebugModeSer
  , RNGs(..)
  , ActorTime, updateActorTime, ageActor
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import qualified System.Random as R

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ItemRev

-- | Global, server state.
data StateServer = StateServer
  { sactorTime    :: !ActorTime         -- ^ absolute times of next actions
  , sdiscoKind    :: !DiscoveryKind     -- ^ full item kind discoveries data
  , sdiscoKindRev :: !DiscoveryKindRev  -- ^ reverse map, used for item creation
  , suniqueSet    :: !UniqueSet         -- ^ already generated unique items
  , sdiscoAspect  :: !DiscoveryAspect   -- ^ full item aspect data
  , sitemSeedD    :: !ItemSeedDict  -- ^ map from item ids to item seeds
  , sitemRev      :: !ItemRev       -- ^ reverse id map, used for item creation
  , sflavour      :: !FlavourMap    -- ^ association of flavour to items
  , sacounter     :: !ActorId       -- ^ stores next actor index
  , sicounter     :: !ItemId        -- ^ stores next item index
  , snumSpawned   :: !(EM.EnumMap LevelId Int)
  , sundo         :: ![CmdAtomic]   -- ^ atomic commands performed to date
  , sperFid       :: !PerFid        -- ^ perception of all factions
  , sperValidFid  :: !PerValidFid   -- ^ perception validity for all factions
  , sperCacheFid  :: !PerCacheFid   -- ^ perception cache of all factions
  , sactorAspect  :: !ActorAspect   -- ^ full actor aspect data
  , sfovLucidLid  :: !FovLucidLid   -- ^ ambient or shining light positions
  , sfovClearLid  :: !FovClearLid   -- ^ clear tiles positions
  , sfovLitLid    :: !FovLitLid     -- ^ ambient light positions
  , sarenas       :: ![LevelId]     -- ^ active arenas
  , svalidArenas  :: !Bool          -- ^ whether active arenas valid
  , srandom       :: !R.StdGen      -- ^ current random generator
  , srngs         :: !RNGs          -- ^ initial random generators
  , squit         :: !Bool          -- ^ exit the game loop
  , swriteSave    :: !Bool          -- ^ write savegame to a file now
  , sdebugSer     :: !DebugModeSer  -- ^ current debugging mode
  , sdebugNxt     :: !DebugModeSer  -- ^ debugging mode for the next game
  }
  deriving (Show)

-- | Debug commands. See 'Server.debugArgs' for the descriptions.
data DebugModeSer = DebugModeSer
  { sknowMap       :: !Bool
  , sknowEvents    :: !Bool
  , sknowItems     :: !Bool
  , sniffIn        :: !Bool
  , sniffOut       :: !Bool
  , sallClear      :: !Bool
  , sgameMode      :: !(Maybe (GroupName ModeKind))
  , sautomateAll   :: !Bool
  , skeepAutomated :: !Bool
  , sdungeonRng    :: !(Maybe R.StdGen)
  , smainRng       :: !(Maybe R.StdGen)
  , snewGameSer    :: !Bool
  , scurDiffSer    :: !Int
  , sdumpInitRngs  :: !Bool
  , ssavePrefixSer :: !String
  , sdbgMsgSer     :: !Bool
  , sdebugCli      :: !DebugModeCli
      -- The client debug inside server debug only holds the client commandline
      -- options and is never updated with config options, etc.
  }
  deriving Show

data RNGs = RNGs
  { dungeonRandomGenerator  :: !(Maybe R.StdGen)
  , startingRandomGenerator :: !(Maybe R.StdGen)
  }

instance Show RNGs where
  show RNGs{..} =
    let args = [ maybe "" (\gen -> "--setDungeonRng \"" ++ show gen ++ "\"")
                       dungeonRandomGenerator
               , maybe "" (\gen -> "--setMainRng \"" ++ show gen ++ "\"")
                       startingRandomGenerator ]
    in unwords args

type ActorTime =
  EM.EnumMap FactionId (EM.EnumMap LevelId (EM.EnumMap ActorId Time))

updateActorTime :: FactionId -> LevelId -> ActorId -> Time -> ActorTime
                -> ActorTime
updateActorTime !fid !lid !aid !time =
  EM.adjust (EM.adjust (EM.insert aid time) lid) fid

ageActor :: FactionId -> LevelId -> ActorId -> Delta Time -> ActorTime
         -> ActorTime
ageActor !fid !lid !aid !delta =
  EM.adjust (EM.adjust (EM.adjust (flip timeShift delta) aid) lid) fid

-- | Initial, empty game server state.
emptyStateServer :: StateServer
emptyStateServer =
  StateServer
    { sactorTime = EM.empty
    , sdiscoKind = EM.empty
    , sdiscoKindRev = EM.empty
    , suniqueSet = ES.empty
    , sdiscoAspect = EM.empty
    , sitemSeedD = EM.empty
    , sitemRev = HM.empty
    , sflavour = emptyFlavourMap
    , sacounter = toEnum 0
    , sicounter = toEnum 0
    , snumSpawned = EM.empty
    , sundo = []
    , sperFid = EM.empty
    , sperValidFid = EM.empty
    , sperCacheFid = EM.empty
    , sactorAspect = EM.empty
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

defDebugModeSer :: DebugModeSer
defDebugModeSer = DebugModeSer { sknowMap = False
                               , sknowEvents = False
                               , sknowItems = False
                               , sniffIn = False
                               , sniffOut = False
                               , sallClear = False
                               , sgameMode = Nothing
                               , sautomateAll = False
                               , skeepAutomated = False
                               , sdungeonRng = Nothing
                               , smainRng = Nothing
                               , snewGameSer = False
                               , scurDiffSer = difficultyDefault
                               , sdumpInitRngs = False
                               , ssavePrefixSer = "save"
                               , sdbgMsgSer = False
                               , sdebugCli = defDebugModeCli
                               }

instance Binary StateServer where
  put StateServer{..} = do
    put sactorTime
    put sdiscoKind
    put sdiscoKindRev
    put suniqueSet
    put sdiscoAspect
    put sitemSeedD
    put sitemRev
    put sflavour
    put sacounter
    put sicounter
    put snumSpawned
    put sundo
    put (show srandom)
    put srngs
    put sdebugSer
  get = do
    sactorTime <- get
    sdiscoKind <- get
    sdiscoKindRev <- get
    suniqueSet <- get
    sdiscoAspect <- get
    sitemSeedD <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    snumSpawned <- get
    sundo <- get
    g <- get
    srngs <- get
    sdebugSer <- get
    let srandom = read g
        sperFid = EM.empty
        sperValidFid = EM.empty
        sperCacheFid = EM.empty
        sactorAspect = EM.empty
        sfovLucidLid = EM.empty
        sfovClearLid = EM.empty
        sfovLitLid = EM.empty
        sarenas = []
        svalidArenas = False
        squit = False
        swriteSave = False
        sdebugNxt = defDebugModeSer
    return $! StateServer{..}

instance Binary DebugModeSer where
  put DebugModeSer{..} = do
    put sknowMap
    put sknowEvents
    put sknowItems
    put sniffIn
    put sniffOut
    put sallClear
    put sgameMode
    put sautomateAll
    put skeepAutomated
    put scurDiffSer
    put ssavePrefixSer
    put sdbgMsgSer
    put sdebugCli
  get = do
    sknowMap <- get
    sknowEvents <- get
    sknowItems <- get
    sniffIn <- get
    sniffOut <- get
    sallClear <- get
    sgameMode <- get
    sautomateAll <- get
    skeepAutomated <- get
    scurDiffSer <- get
    ssavePrefixSer <- get
    sdbgMsgSer <- get
    sdebugCli <- get
    let sdungeonRng = Nothing
        smainRng = Nothing
        snewGameSer = False
        sdumpInitRngs = False
    return $! DebugModeSer{..}

instance Binary RNGs where
  put RNGs{..} = do
    put (show dungeonRandomGenerator)
    put (show startingRandomGenerator)
  get = do
    dg <- get
    sg <- get
    let dungeonRandomGenerator = read dg
        startingRandomGenerator = read sg
    return $! RNGs{..}
