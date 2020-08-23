{-# LANGUAGE DeriveGeneric #-}
-- | UI client options.
module Game.LambdaHack.Client.UI.UIOptions
  ( UIOptions(..), FontDefinition(..), HintingMode(..), FontSet(..)
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.DeepSeq
import Data.Binary
import GHC.Generics (Generic)

import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import qualified Game.LambdaHack.Definition.Color as Color

-- | Options that affect the UI of the client.
data UIOptions = UIOptions
  { -- commands
    uCommands           :: [(K.KM, CmdTriple)]
    -- hero names
  , uHeroNames          :: [(Int, (Text, Text))]
    -- ui
  , uVi                 :: Bool
  , uLeftHand           :: Bool
  , uChosenFontset      :: Text
  , uGtkFontFamily      :: Text
  , uSdlSquareFontFile  :: Text
  , uSdlPropFontSize    :: Int
  , uSdlPropFontFile    :: Text
  , uSdlMonoFontSize    :: Int
  , uSdlMonoFontFile    :: Text
  , uSdlScalableSizeAdd :: Int
  , uSdlBitmapSizeAdd   :: Int
  , uScalableFontSize   :: Int
  , uHistoryMax         :: Int
  , uMaxFps             :: Double
  , uNoAnim             :: Bool
  , uhpWarningPercent   :: Int
      -- ^ HP percent at which warning is emitted.
  , uMessageColors      :: Maybe [(MsgClass, Color.Color)]
  , uCmdline            :: [String]
      -- ^ Hardwired commandline arguments to process.
  , uFonts              :: [(Text, FontDefinition)]
  , uFontsets           :: [(Text, FontSet)]
  }
  deriving (Show, Generic)

instance NFData UIOptions

instance Binary UIOptions

data FontDefinition =
    FontProportional Text Int HintingMode
  | FontMonospace Text Int HintingMode
  | FontMapScalable Text Int HintingMode Int
  | FontMapBitmap Text Int HintingMode Int
  deriving (Show, Read, Generic)

instance NFData FontDefinition

instance Binary FontDefinition

data HintingMode = HintingHeavy | HintingLight | HintingNotApplicable
  deriving (Show, Read, Generic)

instance NFData HintingMode

instance Binary HintingMode

data FontSet = FontSet
  { fontMapScalable :: Text
  , fontMapBitmap   :: Text
  , fontPropRegular :: Text
  , fontPropBold    :: Text
  , fontMono        :: Text }
  deriving (Show, Read, Generic)

instance NFData FontSet

instance Binary FontSet
