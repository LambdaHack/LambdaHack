-- | Personal game configuration file support.
module Game.LambdaHack.Action.ConfigIO
  ( mkConfigRules, mkConfigUI, dump
  ) where

import qualified Data.Char as Char
import qualified Data.ConfigFile as CF
import Data.List
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath
import qualified System.Random as R

import Game.LambdaHack.Config
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Utils.Assert

overrideCP :: CP -> FilePath -> IO CP
overrideCP cp@(CP defCF) cfile = do
  b <- doesFileExist cfile
  if not b
    then return cp
    else do
      c <- CF.readfile defCF cfile
      return $ toCP $ forceEither c

-- | Read a player configuration file and use it to override
-- options from a default config. Currently we can't unset options,
-- only override. The default config, passed in argument @configDefault@,
-- is expected to come from a default configuration file included via CPP.
-- The player configuration comes from file @cfile@.
mkConfig :: String -> FilePath -> IO CP
mkConfig configDefault cfile = do
  let delComment = map (drop 2) $ init . drop 3 $ lines configDefault
      unConfig = unlines delComment
      -- Evaluate, to catch config errors ASAP.
      !defCF = forceEither $ CF.readstring CF.emptyCP unConfig
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
  then assert `failure`"Overwritten config option: " ++ s ++ "." ++ o
  else CP $ forceEither $ CF.set conf s o v

-- | Gets a random generator from the config or,
-- if not present, generates one and updates the config with it.
getSetGen :: CP      -- ^ config
          -> String  -- ^ name of the generator
          -> IO (R.StdGen, CP)
getSetGen config option =
  case getOption config "engine" option of
    Just sg -> return (read sg, config)
    Nothing -> do
      -- Pick the randomly chosen generator from the IO monad
      -- and record it in the config for debugging (can be 'D'umped).
      g <- R.newStdGen
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

-- | In case of corruption, just fail.
forceEither :: Show a => Either a b -> b
forceEither (Left a)  = assert `failure` a
forceEither (Right b) = b

-- | A simplified access to an option in a given section,
-- with simple error reporting (no internal errors are caught nor hidden).
-- If there is no such option, gives Nothing.
getOption :: CF.Get_C a => CP -> CF.SectionSpec -> CF.OptionSpec -> Maybe a
getOption (CP conf) s o =
  if CF.has_option conf s o
  then Just $ forceEither $ CF.get conf s o
  else Nothing

-- | Simplified access to an option in a given section.
-- Fails if the option is not present.
get :: CF.Get_C a => CP -> CF.SectionSpec -> CF.OptionSpec -> a
get (CP conf) s o =
  if CF.has_option conf s o
  then forceEither $ CF.get conf s o
  else assert `failure` "Unknown config option: " ++ s ++ "." ++ o

-- | An association list corresponding to a section. Fails if no such section.
getItems :: CP -> CF.SectionSpec -> [(String, String)]
getItems (CP conf) s =
  if CF.has_section conf s
  then forceEither $ CF.items conf s
  else assert `failure` "Unknown config section: " ++ s

parseConfigRules :: CP -> Config
parseConfigRules cp =
  let configSelfString = let CP conf = cp in CF.to_string conf
      configCaves = map (\(n, t) -> (T.pack n, T.pack t)) $ getItems cp "caves"
      configDepth = get cp "dungeon" "depth"
      configFovMode = get cp "engine" "fovMode"
      configSmellTimeout = get cp "engine" "smellTimeout"
      configBaseHP = get cp "heroes" "baseHP"
      configExtraHeroes = get cp "heroes" "extraHeroes"
      configFirstDeathEnds = get cp "heroes" "firstDeathEnds"
      configFaction = T.pack $ get cp "heroes" "faction"
  in Config{..}

parseConfigUI :: FilePath -> CP -> ConfigUI
parseConfigUI dataDir cp =
  let mkKey s =
        case K.keyTranslate s of
          K.Unknown _ ->
            assert `failure` ("unknown config file key <" ++ s ++ ">")
          key -> key
      configCommands =
        let mkCommand (key, def) = (mkKey key, def)
            section = getItems cp "commands"
        in map mkCommand section
      configAppDataDir = dataDir
      configDiaryFile = dataDir </> get cp "files" "diaryFile"
      configSaveFile = dataDir </> get cp "files" "saveFile"
      configBkpFile = dataDir </> get cp "files" "saveFile" <.> ".bkp"
      configScoresFile = dataDir </> get cp "files" "scoresFile"
      configRulesCfgFile = dataDir </> "config.rules"
      configUICfgFile = dataDir </> "config.ui"
      configHeroNames =
        let toNumber (ident, name) =
              case stripPrefix "HeroName_" ident of
                Just n -> (read n, T.pack name)
                Nothing -> assert `failure` ("wrong hero name id " ++ ident)
            section = getItems cp "heroNames"
        in map toNumber section
      configMacros =
        let trMacro (from, to) =
              let !fromTr = mkKey from
                  !toTr  = mkKey to
              in if fromTr == toTr
                 then assert `failure` "degenerate alias: " ++ show toTr
                 else (fromTr, toTr)
            section = getItems cp "macros"
        in map trMacro section
      configFont = get cp "ui" "font"
      configHistoryMax = get cp "ui" "historyMax"
  in ConfigUI{..}

-- | Read and parse rules config file and supplement it with random seeds.
mkConfigRules :: Kind.Ops RuleKind -> IO (Config, R.StdGen, R.StdGen)
mkConfigRules corule = do
  let cpRulesDefault = rcfgRulesDefault $ Kind.stdRuleset corule
  appData <- appDataDir
  cpRules <- mkConfig cpRulesDefault $ appData </> "config.rules.ini"
  (dungeonGen,  cp2) <- getSetGen cpRules "dungeonRandomGenerator"
  (startingGen, cp3) <- getSetGen cp2     "startingRandomGenerator"
  return (parseConfigRules cp3, dungeonGen, startingGen)

-- | Read and parse UI config file.
mkConfigUI :: Kind.Ops RuleKind -> IO ConfigUI
mkConfigUI corule = do
  let cpUIDefault = rcfgUIDefault $ Kind.stdRuleset corule
  appData <- appDataDir
  cpUI <- mkConfig cpUIDefault $ appData </> "config.ui.ini"
  return $ parseConfigUI appData cpUI
