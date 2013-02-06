{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), defStateServer
  , DebugModeSer(..), cycleTryFov
  ) where

import Data.Binary
import qualified Data.HashMap.Strict as HM
import Data.Typeable

import Game.LambdaHack.Actor
import Game.LambdaHack.Item
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov

-- | Global, server state.
data StateServer = StateServer
  { sdiscoRev :: !DiscoRev      -- ^ reverse disco map, used for item creation
  , sitemRev  :: !ItemRev       -- ^ reverse id map, used for item creation
  , sflavour  :: !FlavourMap    -- ^ association of flavour to items
  , sacounter :: !ActorId       -- ^ stores next actor index
  , sicounter :: !ItemId        -- ^ stores next item index
  , sconfig   :: !Config        -- ^ this game's config (including initial RNG)
  , sdebugSer :: !DebugModeSer  -- ^ debugging mode
  }
  deriving (Show, Typeable)

data DebugModeSer = DebugModeSer
  { stryFov :: !(Maybe FovMode) }
  deriving Show

-- | Initial game server state.
defStateServer :: DiscoRev -> FlavourMap -> Config -> StateServer
defStateServer sdiscoRev sflavour sconfig =
  StateServer
    { sacounter = toEnum 0
    , sicounter = toEnum 0
    , sdebugSer = defDebugModeSer
    , sitemRev = HM.empty
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
    put sconfig
  get = do
    sdiscoRev <- get
    sitemRev <- get
    sflavour <- get
    sacounter <- get
    sicounter <- get
    sconfig <- get
    let sdebugSer = defDebugModeSer
    return StateServer{..}
