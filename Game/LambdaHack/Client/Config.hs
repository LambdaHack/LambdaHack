-- | Personal game configuration file type definitions.
module Game.LambdaHack.Client.Config
  ( ConfigUI(..)
  ) where

import Control.DeepSeq

import Game.LambdaHack.Client.HumanCmd
import qualified Game.LambdaHack.Common.Key as K

-- | Fully typed contents of the UI config file. This config
-- is a part of a game client.
data ConfigUI = ConfigUI
  { -- commands
    configCommands    :: ![(K.KM, HumanCmd)]
    -- files
  , configAppDataDir  :: !FilePath
  , configUICfgFile   :: !FilePath
  , configSavePrefix  :: !String
    -- macros
  , configMacros      :: ![(K.KM, K.KM)]
    -- ui
  , configFont        :: !String
  , configHistoryMax  :: !Int
  , configMaxFps      :: !Int
  , configNoAnim      :: !Bool
  , configRunStopMsgs :: !Bool
  }
  deriving Show

instance NFData ConfigUI
