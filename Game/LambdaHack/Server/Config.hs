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
  , configScoresFile     :: !FilePath
  , configRulesCfgFile   :: !FilePath
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
    put configScoresFile
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
    configScoresFile     <- get
    configRulesCfgFile   <- get
    configBaseHP         <- get
    configExtraHeroes    <- get
    configFirstDeathEnds <- get
    configFaction        <- get
    configHeroNames      <- get
    return Config{..}
