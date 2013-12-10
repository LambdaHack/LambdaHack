-- TODO: factor out parts common with Client.ConfigIO
-- | Personal game configuration file support.
module Game.LambdaHack.Server.Action.ConfigIO
  ( mkConfigRules, dump
  ) where

import Control.DeepSeq
import qualified Data.Char as Char
import qualified Data.ConfigFile as CF
import Data.List
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath
import qualified System.Random as R

import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.Config
import Control.Exception.Assert.Sugar

-- TODO: Refactor the client and server ConfigIO.hs, after
-- https://github.com/kosmikus/LambdaHack/issues/45.

overrideCP :: CP -> FilePath -> IO CP
overrideCP cp@(CP defCF) cfile = do
  cpExists <- doesFileExist cfile
  if not cpExists
    then return cp
    else do
      c <- CF.readfile defCF cfile
      return $ toCP $ assert `forceEither` c

-- | Read a player configuration file and use it to override
-- options from a default config. Currently we can't unset options,
-- only override. The default config, passed in argument @configDefault@,
-- is expected to come from a default configuration file included via TH.
-- The player configuration comes from file @cfile@.
mkConfig :: String -> FilePath -> IO CP
mkConfig configDefault cfile = do
  let delComment = map (drop 2) $ lines configDefault
      unConfig = unlines delComment
      -- Evaluate, to catch config errors ASAP.
      !defCF = assert `forceEither` CF.readstring CF.emptyCP unConfig
      !defCP = toCP defCF
  overrideCP defCP cfile

-- | Personal data directory for the game. Depends on the OS and the game,
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name

-- | Dumps the current configuration to a file.
dump :: Config -> FilePath -> IO ()
dump Config{configSelfString} fn = do
  current <- getCurrentDirectory
  let path = current </> fn
  writeFile path configSelfString

-- | Simplified setting of an option in a given section. Overwriting forbidden.
set :: CP -> CF.SectionSpec -> CF.OptionSpec -> String -> CP
set (CP conf) s o v =
  if CF.has_option conf s o
  then assert `failure`"overwritten config option" `twith` (s, o)
  else CP $ assert `forceEither` CF.set conf s o v

-- | Gets a random generator from the config or,
-- if not present, generates one and updates the config with it.
getSetGen :: CP      -- ^ config
          -> String  -- ^ name of the generator
          -> Maybe R.StdGen
          -> IO (R.StdGen, CP)
getSetGen config option mrandom =
  case getOption config "engine" option of
    Just sg -> return (read sg, config)
    Nothing -> do
      -- Pick the randomly chosen generator from the IO monad (unless given)
      -- and record it in the config for debugging (can be 'D'umped).
      g <- case mrandom of
        Just rnd -> return rnd
        Nothing -> R.newStdGen
      let gs = show g
          c = set config "engine" option gs
      return (g, c)

-- | The content of the configuration file. It's parsed
-- in a case sensitive way (unlike by default in ConfigFile).
newtype CP = CP CF.ConfigParser

instance Show CP where
  show (CP conf) = show $ CF.to_string conf

-- | Switches all names to case sensitive (unlike by default in
-- the "ConfigFile" library) and wraps in the constructor.
toCP :: CF.ConfigParser -> CP
toCP cf = CP $ cf {CF.optionxform = id}

-- | A simplified access to an option in a given section,
-- with simple error reporting (no internal errors are caught nor hidden).
-- If there is no such option, gives Nothing.
getOption :: CF.Get_C a => CP -> CF.SectionSpec -> CF.OptionSpec -> Maybe a
getOption (CP conf) s o =
  if CF.has_option conf s o
  then Just $ assert `forceEither` CF.get conf s o
  else Nothing

-- | Simplified access to an option in a given section.
-- Fails if the option is not present.
get :: CF.Get_C a => CP -> CF.SectionSpec -> CF.OptionSpec -> a
get (CP conf) s o =
  if CF.has_option conf s o
  then assert `forceEither` CF.get conf s o
  else assert `failure` "unknown CF option" `twith` (s, o, CF.to_string conf)

-- | An association list corresponding to a section. Fails if no such section.
getItems :: CP -> CF.SectionSpec -> [(String, String)]
getItems (CP conf) s =
  if CF.has_section conf s
  then assert `forceEither` CF.items conf s
  else assert `failure` "unknown CF section" `twith` (s, CF.to_string conf)

parseConfigRules :: FilePath -> CP -> Config
parseConfigRules dataDir cp =
  let configSelfString = let CP conf = cp in CF.to_string conf
      configFirstDeathEnds = get cp "engine" "firstDeathEnds"
      configFovMode = get cp "engine" "fovMode"
      configSaveBkpClips = get cp "engine" "saveBkpClips"
      configAppDataDir = dataDir
      configScoresFile = dataDir </> get cp "file" "scoresFile"
      configRulesCfgFile = dataDir </> "config.rules"
      configSavePrefix = get cp "file" "savePrefix"
      configHeroNames =
        let toNumber (ident, name) =
              case stripPrefix "HeroName_" ident of
                Just n -> (read n, T.pack name)
                Nothing -> assert `failure` "wrong hero name id" `twith` ident
            section = getItems cp "heroName"
        in map toNumber section
  in Config{..}

-- | Read and parse rules config file and supplement it with random seeds.
mkConfigRules :: Kind.Ops RuleKind -> Maybe R.StdGen
              -> IO (Config, R.StdGen, R.StdGen)
mkConfigRules corule mrandom = do
  let cpRulesDefault = rcfgRulesDefault $ Kind.stdRuleset corule
  appData <- appDataDir
  cpRules <- mkConfig cpRulesDefault $ appData </> "config.rules.ini"
  (dungeonGen,  cp2) <- getSetGen cpRules "dungeonRandomGenerator" mrandom
  (startingGen, cp3) <- getSetGen cp2     "startingRandomGenerator" mrandom
  let conf = parseConfigRules appData cp3
      -- Catch syntax errors ASAP.
      !res = deepseq conf (conf, dungeonGen, startingGen)
  return res
