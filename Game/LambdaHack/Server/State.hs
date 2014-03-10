-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), emptyStateServer
  , DebugModeSer(..), defDebugModeSer
  , RNGs(..)
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Text (Text)
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.RuleKind

-- | Global, server state.
data StateServer = StateServer
  { sdisco     :: !Discovery     -- ^ full item discoveries data
  , sdiscoRev  :: !DiscoRev      -- ^ reverse disco map, used for item creation
  , sitemRev   :: !ItemRev       -- ^ reverse id map, used for item creation
  , sflavour   :: !FlavourMap    -- ^ association of flavour to items
  , sacounter  :: !ActorId       -- ^ stores next actor index
  , sicounter  :: !ItemId        -- ^ stores next item index
  , sundo      :: ![Atomic]      -- ^ atomic commands performed to date
  , sper       :: !Pers          -- ^ perception of all factions
  , srandom    :: !R.StdGen      -- ^ current random generator
  , srngs      :: !RNGs          -- ^ initial random generators
  , squit      :: !Bool          -- ^ exit the game loop
  , sbkpSave   :: !Bool          -- ^ make backup savefile now
  , sstart     :: !ClockTime     -- ^ this session start time
  , sgstart    :: !ClockTime     -- ^ this game start time
  , sallTime   :: !Time          -- ^ clips since the start of the session
  , sheroNames :: !(EM.EnumMap FactionId [(Int, Text)])
                                 -- ^ hero names sent by clients
  , sdebugSer  :: !DebugModeSer  -- ^ current debugging mode
  , sdebugNxt  :: !DebugModeSer  -- ^ debugging mode for the next game
  }
  deriving (Show)

-- | Debug commands. See 'Server.debugArgs' for the descriptions.
data DebugModeSer = DebugModeSer
  { sknowMap       :: !Bool
  , sknowEvents    :: !Bool
  , sniffIn        :: !Bool
  , sniffOut       :: !Bool
  , sallClear      :: !Bool
  , sgameMode      :: !Text
  , sstopAfter     :: !(Maybe Int)
  , sbenchmark     :: !Bool
  , sdungeonRng    :: !(Maybe R.StdGen)
  , smainRng       :: !(Maybe R.StdGen)
  , sfovMode       :: !(Maybe FovMode)
  , snewGameSer    :: !Bool
  , sdifficultySer :: !Int
  , sdumpInitRngs  :: !Bool
  , ssavePrefixSer :: !(Maybe String)
  , sdbgMsgSer     :: !Bool
  , sdebugCli      :: !DebugModeCli
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
    in intercalate " " args

-- | Initial, empty game server state.
emptyStateServer :: StateServer
emptyStateServer =
  StateServer
    { sdisco = EM.empty
    , sdiscoRev = EM.empty
    , sitemRev = HM.empty
    , sflavour = emptyFlavourMap
    , sacounter = toEnum 0
    , sicounter = toEnum 0
    , sundo = []
    , sper = EM.empty
    , srandom = R.mkStdGen 42
    , srngs = RNGs { dungeonRandomGenerator = Nothing
                   , startingRandomGenerator = Nothing }
    , squit = False
    , sbkpSave = False
    , sstart = TOD 0 0
    , sgstart = TOD 0 0
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
                               , sgameMode = "campaign"
                               , sstopAfter = Nothing
                               , sbenchmark = False
                               , sdungeonRng = Nothing
                               , smainRng = Nothing
                               , sfovMode = Nothing
                               , snewGameSer = False
                               , sdifficultySer = difficultyDefault
                               , sdumpInitRngs = False
                               , ssavePrefixSer = Nothing
                               , sdbgMsgSer = False
                               , sdebugCli = defDebugModeCli
                               }

instance Binary StateServer where
  put StateServer{..} = do
    put sdisco
    put sdiscoRev
    put sitemRev
    put sflavour
    put sacounter
    put sicounter
    put sundo
    put (show srandom)
    put srngs
    put sheroNames
    put sdebugSer
  get = do
    sdisco <- get
    sdiscoRev <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    sundo <- get
    g <- get
    srngs <- get
    sheroNames <- get
    sdebugSer <- get
    let srandom = read g
        sper = EM.empty
        squit = False
        sbkpSave = False
        sstart = TOD 0 0
        sgstart = TOD 0 0
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
    put sdifficultySer
    put sfovMode
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
    sdifficultySer <- get
    sfovMode <- get
    ssavePrefixSer <- get
    sdbgMsgSer <- get
    sdebugCli <- get
    let sstopAfter = Nothing
        sbenchmark = False
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
