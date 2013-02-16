{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), defStateServer
  , DebugModeSer(..), cycleTryFov
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
  { sdiscoRev :: !DiscoRev      -- ^ reverse disco map, used for item creation
  , sitemRev  :: !ItemRev       -- ^ reverse id map, used for item creation
  , sflavour  :: !FlavourMap    -- ^ association of flavour to items
  , sacounter :: !ActorId       -- ^ stores next actor index
  , sicounter :: !ItemId        -- ^ stores next item index
  , sper      :: !Pers          -- ^ perception of all factions
  , srandom   :: !R.StdGen      -- ^ current random generator
  , sconfig   :: !Config        -- ^ this game's config (including initial RNG)
  , squit     :: !Bool          -- ^ will exit the game soon
  , sdebugSer :: !DebugModeSer  -- ^ debugging mode
  }
  deriving (Show, Typeable)

data DebugModeSer = DebugModeSer
  { stryFov :: !(Maybe FovMode) }
  deriving Show

-- | Initial game server state.
defStateServer :: DiscoRev -> FlavourMap -> R.StdGen -> Config -> StateServer
defStateServer sdiscoRev sflavour srandom sconfig =
  StateServer
    { sitemRev = HM.empty
    , sacounter = toEnum 0
    , sicounter = toEnum 0
    , sper = EM.empty
    , squit = False
    , sdebugSer = defDebugModeSer
    , ..
    }

defDebugModeSer :: DebugModeSer
defDebugModeSer = DebugModeSer {stryFov = Nothing}

cycleTryFov :: StateServer -> StateServer
cycleTryFov s@StateServer{sdebugSer=sdebugSer@DebugModeSer{stryFov}} =
  s {sdebugSer = sdebugSer {stryFov = case stryFov of
                               Nothing          -> Just (Digital 100)
                               Just (Digital _) -> Just Permissive
                               Just Permissive  -> Just Shadow
                               Just Shadow      -> Just Blind
                               Just Blind       -> Nothing }}

instance Binary StateServer where
  put StateServer{..} = do
    put sdiscoRev
    put sitemRev
    put sflavour
    put sacounter
    put sicounter
    put (show srandom)
    put sconfig
    put squit
  get = do
    sdiscoRev <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    g <- get
    sconfig <- get
    squit <- get
    let srandom = read g
        sper = EM.empty
        sdebugSer = defDebugModeSer
    return StateServer{..}
