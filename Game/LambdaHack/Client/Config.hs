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
    configCommands   :: ![(K.KM, HumanCmd)]
    -- files
  , configAppDataDir :: !FilePath
  , configUICfgFile  :: !FilePath
  , configSavePrefix :: !String
    -- macros
  , configMacros     :: ![(K.KM, K.KM)]
    -- ui
  , configFont       :: !String
  , configHistoryMax :: !Int
  , configMaxFps     :: !Int
  , configNoAnim     :: !Bool
  }
  deriving Show

instance NFData ConfigUI

instance Binary ConfigUI where
  put ConfigUI{..} = do
    put configCommands
    put configAppDataDir
    put configUICfgFile
    put configSavePrefix
    put configMacros
    put configFont
    put configHistoryMax
    put configMaxFps
    put configNoAnim
  get = do
    configCommands <- get
    configAppDataDir <- get
    configUICfgFile <- get
    configSavePrefix <- get
    configMacros <- get
    configFont <- get
    configHistoryMax <- get
    configMaxFps <- get
    configNoAnim <- get
    return ConfigUI{..}
