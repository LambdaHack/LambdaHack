-- | Personal game configuration file type definitions.
module Game.LambdaHack.Server.Config
  ( Config(..), Caves, Players(..), Player(..), Scenario(..)
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
    -- engine
  , configFirstDeathEnds :: !Bool
  , configFovMode        :: !FovMode
  , configSaveBkpClips   :: !Int
    -- files
  , configAppDataDir     :: !FilePath
  , configScoresFile     :: !FilePath
  , configRulesCfgFile   :: !FilePath
    -- heroNames
  , configHeroNames      :: ![(Int, Text)]
    -- players
  , configPlayers        :: !(Map Text Players)
    -- scenario
  , configScenario       :: !(Map Text Scenario)
  }
  deriving Show

type Caves = EM.EnumMap LevelId (Text, Bool)

data Players = Players
  { playersHuman    :: [Player]
  , playersComputer :: [Player]
  , playersEnemy    :: [(Text, Text)]
  , playersAlly     :: [(Text, Text)]
  }
  deriving (Show, Read)

data Player = Player
  { playerName    :: Text
  , playerKind    :: Text
  , playerInitial :: Int
  , playerEntry   :: LevelId
  }
  deriving (Show, Read)

instance NFData Player

instance Binary Player where
  put Player{..} = do
    put playerName
    put playerKind
    put playerInitial
    put playerEntry
  get = do
    playerName <- get
    playerKind <- get
    playerInitial <- get
    playerEntry <- get
    return Player{..}

instance NFData Players

instance Binary Players where
  put Players{..} = do
    put playersHuman
    put playersComputer
    put playersEnemy
    put playersAlly
  get = do
    playersHuman <- get
    playersComputer <- get
    playersEnemy <- get
    playersAlly <- get
    return Players{..}

data Scenario = Scenario
  { scenarioPlayers :: Text
  , scenarioDungeon :: Text
  }
  deriving (Show, Read)

instance NFData Scenario

instance Binary Scenario where
  put Scenario{..} = do
    put scenarioPlayers
    put scenarioDungeon
  get = do
    scenarioPlayers <- get
    scenarioDungeon <- get
    return Scenario{..}

instance NFData Config

instance Binary Config where
  put Config{..} = do
    put configSelfString
    put configCaves
    put configFirstDeathEnds
    put configFovMode
    put configSaveBkpClips
    put configAppDataDir
    put configScoresFile
    put configRulesCfgFile
    put configHeroNames
    put configPlayers
    put configScenario
  get = do
    configSelfString     <- get
    configCaves          <- get
    configFirstDeathEnds <- get
    configFovMode        <- get
    configSaveBkpClips <- get
    configAppDataDir     <- get
    configScoresFile     <- get
    configRulesCfgFile   <- get
    configHeroNames      <- get
    configPlayers        <- get
    configScenario       <- get
    return Config{..}
