-- | Personal game configuration file support.
module Game.LambdaHack.Client.Action.ConfigIO
  ( mkConfigUI
  ) where

import Control.DeepSeq
import qualified Data.Char as Char
import qualified Data.ConfigFile as CF
import System.Directory
import System.Environment
import System.FilePath

import Game.LambdaHack.Client.Config
import qualified Game.LambdaHack.Client.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Content.RuleKind
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

parseConfigUI :: FilePath -> CP -> ConfigUI
parseConfigUI dataDir cp =
  let mkKey s =
        case K.keyTranslate s of
          K.Unknown _ ->
            assert `failure` ("unknown config file key <" ++ s ++ ">")
          key -> key
      mkKM ('C':'T':'R':'L':'-':s) = K.KM {key=mkKey s, modifier=K.Control}
      mkKM s = K.KM {key=mkKey s, modifier=K.NoModifier}
      configCommands =
        let mkCommand (key, def) = (mkKM key, def)
            section = getItems cp "commands"
        in map mkCommand section
      configAppDataDirUI = dataDir
      configUICfgFile = dataDir </> "config.ui"
      configMacros =
        let trMacro (from, to) =
              let fromTr = mkKM from
                  toTr  = mkKM to
              in if fromTr == toTr
                 then assert `failure` "degenerate alias: " ++ show toTr
                 else (fromTr, toTr)
            section = getItems cp "macros"
        in map trMacro section
      configFont = get cp "ui" "font"
      configHistoryMax = get cp "ui" "historyMax"
  in ConfigUI{..}

-- | Read and parse UI config file.
mkConfigUI :: Kind.Ops RuleKind -> IO ConfigUI
mkConfigUI corule = do
  let cpUIDefault = rcfgUIDefault $ Kind.stdRuleset corule
  appData <- appDataDir
  cpUI <- mkConfig cpUIDefault $ appData </> "config.ui.ini"
  let conf = parseConfigUI appData cpUI
  -- Catch syntax errors ASAP,
  return $! deepseq conf conf
