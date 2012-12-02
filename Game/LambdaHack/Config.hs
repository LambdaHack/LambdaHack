-- | Personal game configuration file support.
module Game.LambdaHack.Config
  ( Config(..)
  ) where

import Data.Text (Text)
import Data.Binary

import qualified Game.LambdaHack.Key as K

-- | Fully typed contents of the config file.
data Config = Config
  { -- commands
    configCommands       :: ![(K.Key, String)]  -- TODO: define Binary Cmd
    -- dungeon
  , configDepth          :: !Int
    -- caves
  , configCaves          :: ![(Text, Text)]
    -- engine
  , configFovMode        :: !Text  -- TODO
  , configFovRadius      :: !Int
    -- heroes
  , configBaseHP         :: !Int
  , configExtraHeroes    :: !Int
  , configFirstDeathEnds :: !Bool
  , configFaction        :: !Text
    -- heroNames
  , configHeroNames      :: ![(Int, Text)]
    -- macros
  , configMacros         :: ![(K.Key, K.Key)]
    -- monsters
  , configSmellTimeout   :: !Int
    -- ui
  , configFont           :: !String
  , configHistoryMax     :: !Int
    -- files
  , configAppDataDir     :: !FilePath
  , configDiaryFile      :: !FilePath
  , configSaveFile       :: !FilePath
  , configBkpFile        :: !FilePath
  , configScoresFile     :: !FilePath
  } deriving Show

instance Binary Config where
  put Config{..} = do
    put configCommands
    put configCaves
    put configDepth
    put configFovMode
    put configFovRadius
    put configHeroNames
    put configBaseHP
    put configExtraHeroes
    put configFirstDeathEnds
    put configFaction
    put configMacros
    put configSmellTimeout
    put configFont
    put configHistoryMax
    put configAppDataDir
    put configDiaryFile
    put configSaveFile
    put configBkpFile
    put configScoresFile
  get = do
    configCommands       <- get
    configCaves          <- get
    configDepth          <- get
    configFovMode        <- get
    configFovRadius      <- get
    configHeroNames      <- get
    configBaseHP         <- get
    configExtraHeroes    <- get
    configFirstDeathEnds <- get
    configFaction        <- get
    configMacros         <- get
    configSmellTimeout   <- get
    configFont           <- get
    configHistoryMax     <- get
    configAppDataDir     <- get
    configDiaryFile      <- get
    configSaveFile       <- get
    configBkpFile        <- get
    configScoresFile     <- get
    return Config{..}
