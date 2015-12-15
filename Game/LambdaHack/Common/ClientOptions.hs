{-# LANGUAGE DeriveGeneric #-}
-- | Screen frames and animations.
module Game.LambdaHack.Common.ClientOptions
  ( DebugModeCli(..), defDebugModeCli
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

data DebugModeCli = DebugModeCli
  { sfont           :: !(Maybe Text)
      -- ^ Font to use for the main game window.
  , scolorIsBold    :: !(Maybe Bool)
      -- ^ Whether to use bold attribute for colorful characters.
  , smaxFps         :: !(Maybe Int)
      -- ^ Maximal frames per second.
      -- This is better low and fixed, to avoid jerkiness and delays
      -- that tell the player there are many intelligent enemies on the level.
      -- That's better than scaling AI sofistication down based
      -- on the FPS setting and machine speed.
  , sdisableAutoYes :: !Bool
      -- ^ Never auto-answer all prompts, even if under AI control.
  , snoAnim         :: !(Maybe Bool)
      -- ^ Don't show any animations.
  , snewGameCli     :: !Bool
      -- ^ Start a new game, overwriting the save file.
  , sbenchmark      :: !Bool
      -- ^ Don't create directories and files and show time stats.
  , stitle          :: !(Maybe Text)
  , saddress        :: !(Maybe Text)
  , ssavePrefixCli  :: !String
      -- ^ Prefix of the save game file name.
  , sfrontendStd    :: !Bool
      -- ^ Whether to use the stdout/stdin frontend for all clients.
  , sfrontendNull   :: !Bool
      -- ^ Whether to use void (no input/output) frontend for all clients.
  , sdbgMsgCli      :: !Bool
      -- ^ Show clients' internal debug messages.
  }
  deriving (Show, Eq, Generic)

instance Binary DebugModeCli

defDebugModeCli :: DebugModeCli
defDebugModeCli = DebugModeCli
  { sfont = Nothing
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
  , sdbgMsgCli = False
  }
