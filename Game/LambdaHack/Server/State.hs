{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Server and client game state types and operations.
module Game.LambdaHack.Server.State
  ( StateServer(..), defStateServer
  , DebugModeSer(..), cycleTryFov
  ) where

import Data.Binary
import Data.Typeable

import Game.LambdaHack.Item
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.Fov

-- | Global, server state.
data StateServer = StateServer
  { sdiscoRev :: !DiscoRev      -- ^ reverse map, used for item creation
  , sflavour  :: !FlavourMap    -- ^ association of flavour to items
  , scounter  :: !Int           -- ^ stores next actor index
  , sconfig   :: !Config        -- ^ this game's config (including initial RNG)
  , squit     :: !(Maybe Bool)  -- ^ just going to save the game
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
    { scounter  = 0
    , squit     = Nothing
    , sdebugSer = defDebugModeSer
    , ..
    }

defDebugModeSer :: DebugModeSer
defDebugModeSer = DebugModeSer
  { stryFov = Nothing }

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
    put sflavour
    put scounter
    put sconfig
    put squit
  get = do
    sdiscoRev <- get
    sflavour <- get
    scounter <- get
    sconfig <- get
    squit <- get
    let sdebugSer = defDebugModeSer
    return StateServer{..}
