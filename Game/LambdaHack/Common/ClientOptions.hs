{-# LANGUAGE DeriveGeneric #-}
-- | Screen frames and animations.
module Game.LambdaHack.Common.ClientOptions
  ( DebugModeCli(..), defDebugModeCli
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import GHC.Generics (Generic)

data DebugModeCli = DebugModeCli
  { sgtkFontFamily    :: !(Maybe Text)
      -- ^ Font family to use for the GTK main game window.
  , sdlFontFile       :: !(Maybe Text)
      -- ^ Font file to use for the SDL2 main game window.
  , sfontSize         :: !(Maybe Int)
      -- ^ Font size to use for the main game window.
  , scolorIsBold      :: !(Maybe Bool)
      -- ^ Whether to use bold attribute for colorful characters.
  , smaxFps           :: !(Maybe Int)
      -- ^ Maximal frames per second.
      -- This is better low and fixed, to avoid jerkiness and delays
      -- that tell the player there are many intelligent enemies on the level.
      -- That's better than scaling AI sofistication down based
      -- on the FPS setting and machine speed.
  , sdisableAutoYes   :: !Bool
      -- ^ Never auto-answer all prompts, even if under AI control.
  , snoAnim           :: !(Maybe Bool)
      -- ^ Don't show any animations.
  , snewGameCli       :: !Bool
      -- ^ Start a new game, overwriting the save file.
  , sbenchmark        :: !Bool
      -- ^ Don't create directories and files and show time stats.
  , stitle            :: !(Maybe Text)
  , saddress          :: !(Maybe Text)
  , ssavePrefixCli    :: !String
      -- ^ Prefix of the save game file name.
  , sfrontendStd      :: !Bool
      -- ^ Whether to use the stdout/stdin frontend.
  , sfrontendNull     :: !Bool
      -- ^ Whether to use null (no input/output) frontend.
  , sfrontendLazy     :: !Bool
      -- ^ Whether to use lazy (output not even calculated) frontend.
  , sdbgMsgCli        :: !Bool
      -- ^ Show clients' internal debug messages.
  , sstopAfterSeconds :: !(Maybe Int)
  , sstopAfterFrames  :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

instance Binary DebugModeCli

defDebugModeCli :: DebugModeCli
defDebugModeCli = DebugModeCli
  { sgtkFontFamily = Nothing
  , sdlFontFile = Nothing
  , sfontSize = Nothing
  , scolorIsBold = Nothing
  , smaxFps = Nothing
  , sdisableAutoYes = False
  , snoAnim = Nothing
  , snewGameCli = False
  , sbenchmark = False
  , stitle = Nothing
  , saddress = Nothing
  , ssavePrefixCli = "save"
  , sfrontendStd = False
  , sfrontendNull = False
  , sfrontendLazy = False
  , sdbgMsgCli = False
  , sstopAfterSeconds = Nothing
  , sstopAfterFrames = Nothing
  }
