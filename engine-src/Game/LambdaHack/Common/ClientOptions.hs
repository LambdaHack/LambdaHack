{-# LANGUAGE DeriveGeneric #-}
-- | Options that affect the behaviour of the client.
module Game.LambdaHack.Common.ClientOptions
  ( FullscreenMode(..), ClientOptions(..), defClientOptions
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Misc

-- | Kinds of fullscreen or windowed mode. See <https://hackage.haskell.org/package/sdl2-2.5.3.0/docs/SDL-Video.html#t:WindowMode>.
data FullscreenMode =
    NotFullscreen        -- ^ a normal window instead of fullscreen
  | BigBorderlessWindow  -- ^ fake fullscreen; window the size of the desktop;
                         --   this is the preferred one, if it works
  | ModeChange           -- ^ real fullscreen with a video mode change
  deriving (Show, Read, Eq, Generic)

instance Binary FullscreenMode

-- | Options that affect the behaviour of the client (but not game rules).
data ClientOptions = ClientOptions
  { schosenFontset    :: Maybe Text
      -- ^ Font set chosen by the player for the whole UI.
  , sallFontsScale    :: Maybe Double
      -- ^ The scale applied to all fonts, resizing the whole UI.
  , sfonts            :: [(Text, FontDefinition)]
      -- ^ Available fonts as defined in config file.
  , sfontsets         :: [(Text, FontSet)]
      -- ^ Available font sets as defined in config file.
  , sfullscreenMode   :: Maybe FullscreenMode
      -- ^ Whether to start in fullscreen mode and in which one.
  , slogPriority      :: Maybe Int
      -- ^ How much to log (e.g., from SDL). 1 is all, 5 is errors, the default.
  , smaxFps           :: Maybe Double
      -- ^ Maximal frames per second.
      -- This is better low and fixed, to avoid jerkiness and delays
      -- that tell the player there are many intelligent enemies on the level.
      -- That's better than scaling AI sofistication down based
      -- on the FPS setting and machine speed.
  , sdisableAutoYes   :: Bool
      -- ^ Never auto-answer all prompts, even if under AI control.
  , snoAnim           :: Maybe Bool
      -- ^ Don't show any animations.
  , snewGameCli       :: Bool
      -- ^ Start a new game, overwriting the save file.
  , sbenchmark        :: Bool
      -- ^ Don't create directories and files and show time stats.
  , stitle            :: Maybe String
  , ssavePrefixCli    :: String
      -- ^ Prefix of the save game file name.
  , sfrontendTeletype :: Bool
      -- ^ Whether to use the stdout/stdin frontend.
  , sfrontendNull     :: Bool
      -- ^ Whether to use null (no input/output) frontend.
  , sfrontendLazy     :: Bool
      -- ^ Whether to use lazy (output not even calculated) frontend.
  , sdbgMsgCli        :: Bool
      -- ^ Show clients' internal debug messages.
  , sstopAfterSeconds :: Maybe Int
  , sstopAfterFrames  :: Maybe Int
  , sprintEachScreen  :: Bool
  , sexposePlaces     :: Bool
  , sexposeItems      :: Bool
  , sexposeActors     :: Bool
  }
  deriving (Show, Eq, Generic)

instance Binary ClientOptions

-- | Default value of client options.
defClientOptions :: ClientOptions
defClientOptions = ClientOptions
  { schosenFontset = Nothing
  , sallFontsScale = Nothing
  , sfonts = []
  , sfontsets = []
  , sfullscreenMode = Nothing
  , slogPriority = Nothing
  , smaxFps = Nothing
  , sdisableAutoYes = False
  , snoAnim = Nothing
  , snewGameCli = False
  , sbenchmark = False
  , stitle = Nothing
  , ssavePrefixCli = ""
  , sfrontendTeletype = False
  , sfrontendNull = False
  , sfrontendLazy = False
  , sdbgMsgCli = False
  , sstopAfterSeconds = Nothing
  , sstopAfterFrames = Nothing
  , sprintEachScreen = False
  , sexposePlaces = False
  , sexposeItems = False
  , sexposeActors = False
  }
