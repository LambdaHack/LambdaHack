{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), emptyStateServer
  , DebugModeSer(..), defDebugModeSer
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Typeable
import qualified System.Random as R

import Game.LambdaHack.Actor
import Game.LambdaHack.Item
import Game.LambdaHack.Perception
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
  , sper      :: !Pers          -- ^ perception of all factions
  , srandom   :: !R.StdGen      -- ^ current random generator
  , sconfig   :: Config         -- ^ this game's config (including initial RNG)
  , squit     :: !(Maybe Bool)  -- ^ will save and possibly exit the game soon
  , sdebugSer :: !DebugModeSer  -- ^ current debugging mode
  , sdebugNxt :: !DebugModeSer  -- ^ debugging mode for the next game
  }
  deriving (Show, Typeable)

data DebugModeSer = DebugModeSer
  { sknowMap    :: !Bool
  , sknowEvents :: !Bool
  , sniffIn     :: !Bool
  , sniffOut    :: !Bool
  , sallClear   :: !Bool
  , stryFov     :: !(Maybe FovMode)
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
    , sper = EM.empty
    , srandom = R.mkStdGen 42
    , sconfig = undefined
    , squit = Nothing
    , sdebugSer = defDebugModeSer
    , sdebugNxt = defDebugModeSer
    }

defDebugModeSer :: DebugModeSer
defDebugModeSer = DebugModeSer { sknowMap = False
                               , sknowEvents = False
                               , sniffIn = False
                               , sniffOut = False
                               , sallClear = False
                               , stryFov = Nothing
                               }

instance Binary StateServer where
  put StateServer{..} = do
    put sdisco
    put sdiscoRev
    put sitemRev
    put sflavour
    put sacounter
    put sicounter
    put (show srandom)
    put sconfig
    put squit
    put sdebugSer
  get = do
    sdisco <- get
    sdiscoRev <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    g <- get
    sconfig <- get
    squit <- get
    sdebugSer <- get
    let srandom = read g
        sper = EM.empty
        sdebugNxt = defDebugModeSer
    return StateServer{..}

instance Binary DebugModeSer where
  put DebugModeSer{..} = do
    put sknowMap
    put sknowEvents
    put sniffIn
    put sniffOut
    put sallClear
    put stryFov
  get = do
    sknowMap <- get
    sknowEvents <- get
    sniffIn <- get
    sniffOut <- get
    sallClear <- get
    stryFov <- get
    return DebugModeSer{..}
