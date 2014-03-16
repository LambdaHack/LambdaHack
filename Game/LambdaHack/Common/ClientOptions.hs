{-# LANGUAGE DeriveGeneric #-}
-- | Screen frames and animations.
module Game.LambdaHack.Common.ClientOptions
  ( DebugModeCli(..), defDebugModeCli
  ) where

import Data.Binary
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Faction

data DebugModeCli = DebugModeCli
  { sfont          :: !(Maybe String)
      -- ^ Font to use for the main game window.
  , smaxFps        :: !(Maybe Int)
      -- ^ Maximal frames per second.
      -- This is better low and fixed, to avoid jerkiness and delays
      -- that tell the player there are many intelligent enemies on the level.
      -- That's better than scaling AI sofistication down based
      -- on the FPS setting and machine speed.
  , snoDelay       :: !Bool
      -- ^ Don't maintain any requested delays between frames,
      -- e.g., for screensaver.
  , snoMore        :: !Bool
      -- ^ Auto-answer all prompts, e.g., for screensaver.
  , snoAnim        :: !(Maybe Bool)
      -- ^ Don't show any animations.
  , snewGameCli    :: !Bool
      -- ^ Start a new game, overwriting the save file.
  , sdifficultyCli :: !Int
      -- ^ The difficulty level for all UI clients.
  , ssavePrefixCli :: !(Maybe String)
      -- ^ Prefix of the save game file.
  , sfrontendStd   :: !Bool
      -- ^ Whether to use the stdout/stdin frontend for all clients.
  , sfrontendNull  :: !Bool
      -- ^ Whether to use void (no input/output) frontend for all clients.
  , sdbgMsgCli     :: !Bool
      -- ^ Show clients' internal debug messages.
  }
  deriving (Show, Eq, Generic)

instance Binary DebugModeCli

defDebugModeCli :: DebugModeCli
defDebugModeCli = DebugModeCli
  { sfont = Nothing
  , smaxFps = Nothing
  , snoDelay = False
  , snoMore = False
  , snoAnim = Nothing
  , snewGameCli = False
  , sdifficultyCli = difficultyDefault
  , ssavePrefixCli = Nothing
  , sfrontendStd = False
  , sfrontendNull = False
  , sdbgMsgCli = False
  }
