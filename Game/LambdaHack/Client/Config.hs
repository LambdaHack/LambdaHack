-- | Personal game configuration file type definitions.
module Game.LambdaHack.Client.Config
  ( ConfigUI(..)
  ) where

import Data.Binary

import qualified Game.LambdaHack.Client.Key as K

-- | Fully typed contents of the UI config file. This config
-- is a part of a game client.
data ConfigUI = ConfigUI
  { -- commands
    configCommands     :: ![(K.Key, String)]  -- TODO: define Binary Cmd
    -- files
  , configAppDataDirUI :: !FilePath
--  , configHistoryFile  :: !FilePath
  , configUICfgFile    :: !FilePath
    -- macros
  , configMacros       :: ![(K.Key, K.Key)]
    -- ui
  , configFont         :: !String
  , configHistoryMax   :: !Int
  } deriving Show

instance Binary ConfigUI where
  put ConfigUI{..} = do
    put configCommands
    put configAppDataDirUI
--    put configHistoryFile
    put configUICfgFile
    put configMacros
    put configFont
    put configHistoryMax
  get = do
    configCommands     <- get
    configAppDataDirUI <- get
--    configHistoryFile  <- get
    configUICfgFile    <- get
    configMacros       <- get
    configFont         <- get
    configHistoryMax   <- get
    return ConfigUI{..}
