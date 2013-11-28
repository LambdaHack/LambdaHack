-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), emptyStateServer
  , DebugModeSer(..), defDebugModeSer
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified System.Random as R
import System.Time

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov

-- | Global, server state.
data StateServer = StateServer
  { sdisco    :: !Discovery     -- ^ full item discoveries data
  , sdiscoRev :: !DiscoRev      -- ^ reverse disco map, used for item creation
  , sitemRev  :: !ItemRev       -- ^ reverse id map, used for item creation
  , sflavour  :: !FlavourMap    -- ^ association of flavour to items
  , sacounter :: !ActorId       -- ^ stores next actor index
  , sicounter :: !ItemId        -- ^ stores next item index
  , sundo     :: ![Atomic]      -- ^ atomic commands performed to date
  , sper      :: !Pers          -- ^ perception of all factions
  , srandom   :: !R.StdGen      -- ^ current random generator
  , sconfig   :: Config         -- ^ this game's config (including initial RNG)
  , squit     :: !Bool          -- ^ exit the game loop
  , sbkpSave  :: !Bool          -- ^ make backup savefile now
  , sstart    :: !ClockTime     -- ^ this session start time
  , sdebugSer :: !DebugModeSer  -- ^ current debugging mode
  , sdebugNxt :: !DebugModeSer  -- ^ debugging mode for the next game
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
  , sfovMode       :: !(Maybe FovMode)
  , snewGameSer    :: !Bool
  , ssavePrefixSer :: !(Maybe String)
  , sdbgMsgSer     :: !Bool
  , sdebugCli      :: !DebugModeCli
  }
  deriving Show

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
    , sconfig = undefined
    , squit = False
    , sbkpSave = False
    , sstart = TOD 0 0
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
                               , sfovMode = Nothing
                               , snewGameSer = False
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
    put sconfig
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
    sconfig <- get
    sdebugSer <- get
    let srandom = read g
        sper = EM.empty
        squit = False
        sbkpSave = False
        sstart = TOD 0 0
        sdebugNxt = defDebugModeSer
    return StateServer{..}

instance Binary DebugModeSer where
  put DebugModeSer{..} = do
    put sknowMap
    put sknowEvents
    put sniffIn
    put sniffOut
    put sallClear
    put sgameMode
    put sfovMode
    put snewGameSer
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
    sfovMode <- get
    snewGameSer <- get
    ssavePrefixSer <- get
    sdbgMsgSer <- get
    sdebugCli <- get
    let sstopAfter = Nothing
    return DebugModeSer{..}
