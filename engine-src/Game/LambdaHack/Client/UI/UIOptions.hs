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
import           Game.LambdaHack.Definition.Defs

-- | Options that affect the UI of the client, specified in the config file.
-- More documentation is in the default config file.
data UIOptions = UIOptions
  { uCommands         :: [(K.KM, CmdTriple)]
  , uHeroNames        :: [(Int, (Text, Text))]
  , uVi               :: Bool
  , uLeftHand         :: Bool
  , uChosenFontset    :: Text
  , uAllFontsScale    :: Double
  , uFullscreenMode   :: FullscreenMode
  , uhpWarningPercent :: Int
  , uMsgWrapColumn    :: X
  , uHistoryMax       :: Int
  , uMaxFps           :: Double
  , uNoAnim           :: Bool
  , uOverrideCmdline  :: [String]
  , uFonts            :: [(Text, FontDefinition)]
  , uFontsets         :: [(Text, FontSet)]
  , uMessageColors    :: [(String, Color.Color)]
  }
  deriving (Show, Generic)

instance NFData UIOptions

instance Binary UIOptions
