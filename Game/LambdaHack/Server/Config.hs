-- | Personal game configuration file type definitions.
module Game.LambdaHack.Server.Config
  ( Config(..)
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Text (Text)

import Game.LambdaHack.Common.Msg ()
import Game.LambdaHack.Server.Fov

-- | Fully typed contents of the rules config file. This config
-- is a part of the game server.
data Config = Config
  { configSelfString     :: !String
    -- caves
  , configCaves          :: ![(Text, Text)]
    -- computerPlayers
  , configComputer       :: ![(Text, Text)]
    -- dungeon
  , configDepth          :: !Int
    -- engine
  , configFovMode        :: !FovMode
    -- files
  , configAppDataDir     :: !FilePath
  , configScoresFile     :: !FilePath
  , configRulesCfgFile   :: !FilePath
    -- heroes
  , configExtraHeroes    :: !Int
  , configFirstDeathEnds :: !Bool
    -- humanPlayers
  , configHuman          :: ![(Text, Text)]
    -- heroNames
  , configHeroNames      :: ![(Int, Text)]
  }
  deriving Show

instance NFData Config

instance Binary Config where
  put Config{..} = do
    put configSelfString
    put configCaves
    put configComputer
    put configDepth
    put configFovMode
    put configAppDataDir
    put configScoresFile
    put configRulesCfgFile
    put configExtraHeroes
    put configFirstDeathEnds
    put configHuman
    put configHeroNames
  get = do
    configSelfString     <- get
    configCaves          <- get
    configComputer       <- get
    configDepth          <- get
    configFovMode        <- get
    configAppDataDir     <- get
    configScoresFile     <- get
    configRulesCfgFile   <- get
    configExtraHeroes    <- get
    configFirstDeathEnds <- get
    configHuman          <- get
    configHeroNames      <- get
    return Config{..}
