-- | Personal game configuration file type definitions.
module Game.LambdaHack.Server.Config
  ( Config(..), Players(..)
  ) where

import Control.DeepSeq
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Map.Strict (Map)
import Data.Text (Text)

import Game.LambdaHack.Common.Level
import Game.LambdaHack.Server.Fov

-- | Fully typed contents of the rules config file. This config
-- is a part of the game server.
data Config = Config
  { configSelfString     :: !String
    -- caves
  , configCaves          :: !(Map Text Caves)
    -- players
  , configPlayers        :: !(Map Text Players)
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
    -- heroNames
  , configHeroNames      :: ![(Int, Text)]
  }
  deriving Show

type Caves = EM.EnumMap LevelId Text

data Players = Players
  { playersHuman    :: [(Text, Text)]
  , playersComputer :: [(Text, Text)]
  }
  deriving (Show, Read)

instance NFData Players

instance Binary Players where
  put Players{..} = do
    put playersHuman
    put playersComputer
  get = do
    playersHuman <- get
    playersComputer <- get
    return Players{..}

instance NFData Config

instance Binary Config where
  put Config{..} = do
    put configSelfString
    put configCaves
    put configPlayers
    put configDepth
    put configFovMode
    put configAppDataDir
    put configScoresFile
    put configRulesCfgFile
    put configExtraHeroes
    put configFirstDeathEnds
    put configHeroNames
  get = do
    configSelfString     <- get
    configCaves          <- get
    configPlayers        <- get
    configDepth          <- get
    configFovMode        <- get
    configAppDataDir     <- get
    configScoresFile     <- get
    configRulesCfgFile   <- get
    configExtraHeroes    <- get
    configFirstDeathEnds <- get
    configHeroNames      <- get
    return Config{..}
