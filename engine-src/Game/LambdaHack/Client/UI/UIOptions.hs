{-# LANGUAGE DeriveGeneric #-}
-- | UI client options specified in the config file.
module Game.LambdaHack.Client.UI.UIOptions
  ( UIOptions(..)
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.DeepSeq
import Data.Binary
import GHC.Generics (Generic)

import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Common.ClientOptions (FullscreenMode)
import           Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Definition.Color as Color

-- | Options that affect the UI of the client, specified in the config file. More documentation is in the default config file.
data UIOptions = UIOptions
  { -- commands
    uCommands         :: [(K.KM, CmdTriple)]
    -- hero names
  , uHeroNames        :: [(Int, (Text, Text))]
    -- ui
  , uVi               :: Bool
  , uLeftHand         :: Bool
  , uChosenFontset    :: Text
  , uAllFontsScale    :: Double
  , uScreen1PerLine   :: Bool
  , uHistory1PerLine  :: Bool
  , uHistoryMax       :: Int
  , uMaxFps           :: Double
  , uNoAnim           :: Bool
  , uhpWarningPercent :: Int
      -- ^ HP percent at which warning is emitted.
  , uMessageColors    :: [(String, Color.Color)]
      -- ^ Prefixes of message class constructor names paired with colors.
      --   The first prefix that matches, wins.
  , uCmdline          :: [String]
      -- ^ Hardwired commandline arguments to process.
  , uFonts            :: [(Text, FontDefinition)]
  , uFontsets         :: [(Text, FontSet)]
  , uFullscreenMode   :: FullscreenMode
  }
  deriving (Show, Generic)

instance NFData UIOptions

instance Binary UIOptions
