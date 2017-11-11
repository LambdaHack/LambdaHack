{-# LANGUAGE DeriveGeneric #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.ClientOptions
  ( ClientOptions(..), defClientOptions
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import GHC.Generics (Generic)

-- | Options that affect the behaviour of the client (but not game rules).
data ClientOptions = ClientOptions
  { sgtkFontFamily    :: Maybe Text
      -- ^ Font family to use for the GTK main game window.
  , sdlFontFile       :: Maybe Text
      -- ^ Font file to use for the SDL2 main game window.
  , sdlTtfSizeAdd     :: Maybe Int
      -- ^ Pixels to add to map cells on top of scalable font max glyph height.
  , sdlFonSizeAdd     :: Maybe Int
      -- ^ Pixels to add to map cells on top of .fon font max glyph height.
  , sfontSize         :: Maybe Int
      -- ^ Font size to use for the main game window.
  , scolorIsBold      :: Maybe Bool
      -- ^ Whether to use bold attribute for colorful characters.
  , smaxFps           :: Maybe Int
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
  , stitle            :: Maybe Text
  , sfontDir          :: Maybe FilePath
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
  }
  deriving (Show, Eq, Generic)

instance Binary ClientOptions

-- | Default value of client options.
defClientOptions :: ClientOptions
defClientOptions = ClientOptions
  { sgtkFontFamily = Nothing
  , sdlFontFile = Nothing
  , sdlTtfSizeAdd = Nothing
  , sdlFonSizeAdd = Nothing
  , sfontSize = Nothing
  , scolorIsBold = Nothing
  , smaxFps = Nothing
  , sdisableAutoYes = False
  , snoAnim = Nothing
  , snewGameCli = False
  , sbenchmark = False
  , stitle = Nothing
  , sfontDir = Nothing
  , ssavePrefixCli = ""
  , sfrontendTeletype = False
  , sfrontendNull = False
  , sfrontendLazy = False
  , sdbgMsgCli = False
  , sstopAfterSeconds = Nothing
  , sstopAfterFrames = Nothing
  }
