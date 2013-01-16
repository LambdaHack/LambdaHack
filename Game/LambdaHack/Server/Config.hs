-- | Personal game configuration file type definitions.
module Game.LambdaHack.Server.Config
  ( Config(..)
  ) where

import Data.Binary
import Data.Text (Text)
import Game.LambdaHack.Msg ()
import Game.LambdaHack.Server.Fov

-- | Fully typed contents of the rules config file. This config
-- is a part of the game server.
data Config = Config
  { configSelfString     :: !String
    -- caves
  , configCaves          :: ![(Text, Text)]
    -- dungeon
  , configDepth          :: !Int
    -- engine
  , configFovMode        :: !FovMode
    -- files
  , configAppDataDir     :: !FilePath
  , configSaveFile       :: !FilePath
  , configBkpFile        :: !FilePath
  , configScoresFile     :: !FilePath
  , configRulesCfgFile   :: !FilePath
  -- Temporary hack until all clients save their local state separately.
  , configHistoryFile    :: !FilePath
    -- heroes
  , configBaseHP         :: !Int
  , configExtraHeroes    :: !Int
  , configFirstDeathEnds :: !Bool
  , configFaction        :: !Text
    -- heroNames
  , configHeroNames      :: ![(Int, Text)]
  } deriving Show

instance Binary Config where
  put Config{..} = do
    put configSelfString
    put configCaves
    put configDepth
    put configFovMode
    put configAppDataDir
    put configSaveFile
    put configBkpFile
    put configScoresFile
    put configHistoryFile
    put configRulesCfgFile
    put configBaseHP
    put configExtraHeroes
    put configFirstDeathEnds
    put configFaction
    put configHeroNames
  get = do
    configSelfString     <- get
    configCaves          <- get
    configDepth          <- get
    configFovMode        <- get
    configAppDataDir     <- get
    configSaveFile       <- get
    configBkpFile        <- get
    configScoresFile     <- get
    configRulesCfgFile   <- get
    configHistoryFile  <- get
    configBaseHP         <- get
    configExtraHeroes    <- get
    configFirstDeathEnds <- get
    configFaction        <- get
    configHeroNames      <- get
    return Config{..}
