-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), emptyStateServer
  , DebugModeSer(..), defDebugModeSer
  , RNGs(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock.POSIX
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
import Game.LambdaHack.Server.ItemRev

-- | Global, server state.
data StateServer = StateServer
  { sdiscoKind    :: !DiscoveryKind     -- ^ full item kind discoveries data
  , sdiscoKindRev :: !DiscoveryKindRev  -- ^ reverse map, used for item creation
  , suniqueSet    :: !UniqueSet         -- ^ already generated unique items
  , sdiscoEffect  :: !DiscoveryEffect   -- ^ full item effect&Co data
  , sitemSeedD    :: !ItemSeedDict  -- ^ map from item ids to item seeds
  , sitemRev      :: !ItemRev       -- ^ reverse id map, used for item creation
  , sItemFovCache :: !(EM.EnumMap ItemId FovCache3)
                                    -- ^ (sight, smell, light) aspect bonus
                                    --   of the item; zeroes if not in the map
  , sflavour      :: !FlavourMap    -- ^ association of flavour to items
  , sacounter     :: !ActorId       -- ^ stores next actor index
  , sicounter     :: !ItemId        -- ^ stores next item index
  , snumSpawned   :: !(EM.EnumMap LevelId Int)
  , sundo         :: ![CmdAtomic]   -- ^ atomic commands performed to date
  , sper          :: !Pers          -- ^ perception of all factions
  , slit          :: !PersLit       -- ^ cache of lit tiles, etc.
  , srandom       :: !R.StdGen      -- ^ current random generator
  , srngs         :: !RNGs          -- ^ initial random generators
  , squit         :: !Bool          -- ^ exit the game loop
  , swriteSave    :: !Bool          -- ^ write savegame to a file now
  , sstart        :: !POSIXTime     -- ^ this session start time
  , sgstart       :: !POSIXTime     -- ^ this game start time
  , sallTime      :: !Time          -- ^ clips since the start of the session
  , sheroNames    :: !(EM.EnumMap FactionId [(Int, (Text, Text))])
                                    -- ^ hero names sent by clients
  , sdebugSer     :: !DebugModeSer  -- ^ current debugging mode
  , sdebugNxt     :: !DebugModeSer  -- ^ debugging mode for the next game
  }
  deriving (Show)

-- | Debug commands. See 'Server.debugArgs' for the descriptions.
data DebugModeSer = DebugModeSer
  { sknowMap       :: !Bool
  , sknowEvents    :: !Bool
  , sniffIn        :: !Bool
  , sniffOut       :: !Bool
  , sallClear      :: !Bool
  , sgameMode      :: !(Maybe (GroupName ModeKind))
  , sautomateAll   :: !Bool
  , skeepAutomated :: !Bool
  , sstopAfter     :: !(Maybe Int)
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

-- | Initial, empty game server state.
emptyStateServer :: StateServer
emptyStateServer =
  StateServer
    { sdiscoKind = EM.empty
    , sdiscoKindRev = EM.empty
    , suniqueSet = ES.empty
    , sdiscoEffect = EM.empty
    , sitemSeedD = EM.empty
    , sitemRev = HM.empty
    , sItemFovCache = EM.empty
    , sflavour = emptyFlavourMap
    , sacounter = toEnum 0
    , sicounter = toEnum 0
    , snumSpawned = EM.empty
    , sundo = []
    , sper = Pers EM.empty EM.empty
    , slit = (EM.empty, EM.empty, EM.empty, EM.empty)
    , srandom = R.mkStdGen 42
    , srngs = RNGs { dungeonRandomGenerator = Nothing
                   , startingRandomGenerator = Nothing }
    , squit = False
    , swriteSave = False
    , sstart = 0
    , sgstart = 0
    , sallTime = timeZero
    , sheroNames = EM.empty
    , sdebugSer = defDebugModeSer
    , sdebugNxt = defDebugModeSer
    }

defDebugModeSer :: DebugModeSer
defDebugModeSer = DebugModeSer { sknowMap = False
                               , sknowEvents = False
                               , sniffIn = False
                               , sniffOut = False
                               , sallClear = False
                               , sgameMode = Nothing
                               , sautomateAll = False
                               , skeepAutomated = False
                               , sstopAfter = Nothing
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
    put sdiscoKind
    put sdiscoKindRev
    put suniqueSet
    put sdiscoEffect
    put sitemSeedD
    put sitemRev
    put sItemFovCache  -- out of laziness, but it's small
    put sflavour
    put sacounter
    put sicounter
    put snumSpawned
    put sundo
    put (show srandom)
    put srngs
    put sheroNames
    put sdebugSer
  get = do
    sdiscoKind <- get
    sdiscoKindRev <- get
    suniqueSet <- get
    sdiscoEffect <- get
    sitemSeedD <- get
    sitemRev <- get
    sItemFovCache <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    snumSpawned <- get
    sundo <- get
    g <- get
    srngs <- get
    sheroNames <- get
    sdebugSer <- get
    let srandom = read g
        sper = Pers EM.empty EM.empty
        slit = (EM.empty, EM.empty, EM.empty, EM.empty)
        squit = False
        swriteSave = False
        sstart = 0
        sgstart = 0
        sallTime = timeZero
        sdebugNxt = defDebugModeSer  -- TODO: here difficulty level, etc. from the last session is wiped out
    return $! StateServer{..}

instance Binary DebugModeSer where
  put DebugModeSer{..} = do
    put sknowMap
    put sknowEvents
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
    let sstopAfter = Nothing
        sdungeonRng = Nothing
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
