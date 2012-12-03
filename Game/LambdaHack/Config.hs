-- | Personal game configuration file type definitions.
module Game.LambdaHack.Config
  ( Config(..), ConfigUI(..)
  ) where

import Data.Text (Text)
import Data.Binary

import qualified Game.LambdaHack.Key as K

-- | Fully typed contents of the rules config file.
data Config = Config
  { configSelfString     :: !String
    -- caves
  , configCaves          :: ![(Text, Text)]
    -- dungeon
  , configDepth          :: !Int
    -- engine
  , configFovMode        :: !Text  -- TODO
  , configFovRadius      :: !Int
  , configSmellTimeout   :: !Int
    -- heroes
  , configBaseHP         :: !Int
  , configExtraHeroes    :: !Int
  , configFirstDeathEnds :: !Bool
  , configFaction        :: !Text
  } deriving Show

-- | Fully typed contents of the UI config file.
data ConfigUI = ConfigUI
  { -- commands
    configCommands     :: ![(K.Key, String)]  -- TODO: define Binary Cmd
    -- files
  , configAppDataDir   :: !FilePath
  , configDiaryFile    :: !FilePath
  , configSaveFile     :: !FilePath
  , configBkpFile      :: !FilePath
  , configScoresFile   :: !FilePath
  , configRulesCfgFile :: !FilePath
  , configUICfgFile    :: !FilePath
    -- heroNames
  , configHeroNames    :: ![(Int, Text)]
    -- macros
  , configMacros       :: ![(K.Key, K.Key)]
    -- ui
  , configFont         :: !String
  , configHistoryMax   :: !Int
  } deriving Show

instance Binary Config where
  put Config{..} = do
    put configSelfString
    put configCaves
    put configDepth
    put configFovMode
    put configFovRadius
    put configSmellTimeout
    put configBaseHP
    put configExtraHeroes
    put configFirstDeathEnds
    put configFaction
  get = do
    configSelfString     <- get
    configCaves          <- get
    configDepth          <- get
    configFovMode        <- get
    configFovRadius      <- get
    configSmellTimeout   <- get
    configBaseHP         <- get
    configExtraHeroes    <- get
    configFirstDeathEnds <- get
    configFaction        <- get
    return Config{..}

instance Binary ConfigUI where
  put ConfigUI{..} = do
    put configCommands
    put configAppDataDir
    put configDiaryFile
    put configSaveFile
    put configBkpFile
    put configRulesCfgFile
    put configUICfgFile
    put configScoresFile
    put configHeroNames
    put configMacros
    put configFont
    put configHistoryMax
  get = do
    configCommands     <- get
    configAppDataDir   <- get
    configDiaryFile    <- get
    configSaveFile     <- get
    configBkpFile      <- get
    configScoresFile   <- get
    configRulesCfgFile <- get
    configUICfgFile    <- get
    configHeroNames    <- get
    configMacros       <- get
    configFont         <- get
    configHistoryMax   <- get
    return ConfigUI{..}
