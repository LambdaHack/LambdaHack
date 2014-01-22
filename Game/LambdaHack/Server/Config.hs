-- | Personal game configuration file type definitions.
module Game.LambdaHack.Server.Config
  ( Config(..)
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Text (Text)

import Game.LambdaHack.Server.Fov

-- | Fully typed contents of the rules config file. This config
-- is a part of the game server.
data Config = Config
  { configSelfString     :: !String
    -- engine
  , configFirstDeathEnds :: !Bool
  , configFovMode        :: !FovMode
  , configSaveBkpClips   :: !Int
    -- files
  , configAppDataDir     :: !FilePath
  , configScoresFile     :: !FilePath
  , configRulesCfgFile   :: !FilePath
  , configSavePrefix     :: !String
    -- heroNames
  , configHeroNames      :: ![(Int, Text)]
  }
  deriving Show

instance NFData Config

instance Binary Config where
  put Config{..} = do
    put configSelfString
    put configFirstDeathEnds
    put configFovMode
    put configSaveBkpClips
    put configAppDataDir
    put configScoresFile
    put configRulesCfgFile
    put configSavePrefix
    put configHeroNames
  get = do
    configSelfString     <- get
    configFirstDeathEnds <- get
    configFovMode        <- get
    configSaveBkpClips <- get
    configAppDataDir     <- get
    configScoresFile     <- get
    configRulesCfgFile   <- get
    configSavePrefix <- get
    configHeroNames      <- get
    return $! Config{..}
