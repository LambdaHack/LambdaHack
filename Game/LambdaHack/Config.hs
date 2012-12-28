-- | Personal game configuration file type definitions.
module Game.LambdaHack.Config
  ( Config(..), ConfigUI(..), FovMode(..)
  ) where

import Data.Binary
import Data.Text (Text)

import qualified Game.LambdaHack.Key as K

-- TODO: should Blind really be a FovMode, or a modifier? Let's decide
-- when other similar modifiers are added.
-- | Field Of View scanning mode.
data FovMode =
    Shadow       -- ^ restrictive shadow casting
  | Permissive   -- ^ permissive FOV
  | Digital Int  -- ^ digital FOV with the given radius
  | Blind        -- ^ only feeling out adjacent tiles by touch
  deriving (Show, Read)

-- | Fully typed contents of the rules config file.
data Config = Config
  { configSelfString     :: !String
    -- caves
  , configCaves          :: ![(Text, Text)]
    -- dungeon
  , configDepth          :: !Int
    -- engine
  , configFovMode        :: !FovMode
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
  , configStateClientFile    :: !FilePath
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

instance Binary FovMode where
  put Shadow      = putWord8 0
  put Permissive  = putWord8 1
  put (Digital r) = putWord8 2 >> put r
  put Blind       = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> return Shadow
      1 -> return Permissive
      2 -> fmap Digital get
      3 -> return Blind
      _ -> fail "no parse (FovMode)"

instance Binary Config where
  put Config{..} = do
    put configSelfString
    put configCaves
    put configDepth
    put configFovMode
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
    put configStateClientFile
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
    configStateClientFile    <- get
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
