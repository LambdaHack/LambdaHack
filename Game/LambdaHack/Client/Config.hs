-- | Personal game configuration file type definitions.
module Game.LambdaHack.Client.Config
  ( ConfigUI(..)
  ) where

import Control.DeepSeq
import Data.Binary

import Game.LambdaHack.Client.HumanCmd
import qualified Game.LambdaHack.Common.Key as K

-- | Fully typed contents of the UI config file. This config
-- is a part of a game client.
data ConfigUI = ConfigUI
  { -- commands
    configCommands     :: ![(K.KM, HumanCmd)]
    -- files
  , configAppDataDirUI :: !FilePath
  , configUICfgFile    :: !FilePath
    -- macros
  , configMacros       :: ![(K.KM, K.KM)]
    -- ui
  , configFont         :: !String
  , configHistoryMax   :: !Int
  , configMaxFps       :: !Int
  }
  deriving Show

instance NFData ConfigUI

instance Binary ConfigUI where
  put ConfigUI{..} = do
    put configCommands
    put configAppDataDirUI
    put configUICfgFile
    put configMacros
    put configFont
    put configHistoryMax
    put configMaxFps
  get = do
    configCommands     <- get
    configAppDataDirUI <- get
    configUICfgFile    <- get
    configMacros       <- get
    configFont         <- get
    configHistoryMax   <- get
    configMaxFps <- get
    return ConfigUI{..}
