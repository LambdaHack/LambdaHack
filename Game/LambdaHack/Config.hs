-- | Personal game configuration file support.
module Game.LambdaHack.Config
  ( CP, mkConfig, appDataDir
  , getOption, get, getItems, getFile, dump, getSetGen
  ) where

import System.Directory
import System.FilePath
import System.Environment
import qualified Data.ConfigFile as CF
import qualified Data.Binary as Binary
import qualified Data.Char as Char
import qualified Data.List as L
import qualified System.Random as R

-- | The content of the configuration file. It's parsed
-- in a case sensitive way (unlike by default in ConfigFile).
newtype CP = CP CF.ConfigParser

instance Binary.Binary CP where
  put (CP conf) = Binary.put $ CF.to_string conf
  get = do
    string <- Binary.get
    let c = CF.readstring CF.emptyCP string
    return $ toCP $ forceEither c

instance Show CP where
  show (CP conf) = show $ CF.to_string conf

-- | In case of corruption, just fail.
forceEither :: Show a => Either a b -> b
forceEither (Left a)  = error (show a)
forceEither (Right b) = b

-- | Switches all names to case sensitive (unlike by default in
-- the "ConfigFile" library) and wraps in the constructor.
toCP :: CF.ConfigParser -> CP
toCP cf = CP $ cf {CF.optionxform = id}

overrideCP :: CP -> FilePath -> IO CP
overrideCP (CP defCF) cfile = do
  c <- CF.readfile defCF cfile
  return $ toCP $ forceEither c

-- | Read the player configuration file and use it to override
-- any default config options. Currently we can't unset options, only override.
--
-- The default config, passed in argument @configDefault@,
-- is expected to come from the default configuration file included via CPP
-- in file @ConfigDefault.hs@.
mkConfig :: String -> IO CP
mkConfig configDefault = do
  let delFileMarker = L.init $ L.drop 3 $ lines configDefault
      delComment = L.map (L.drop 2) delFileMarker
      unConfig = unlines delComment
      -- Evaluate, to catch config errors ASAP.
      !defCF = forceEither $ CF.readstring CF.emptyCP unConfig
      !defConfig = toCP defCF
  cfile <- configFile
  b <- doesFileExist cfile
  if not b
    then return defConfig
    else overrideCP defConfig cfile

-- | Personal data directory for the game. Depends on the OS and the game,
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = L.takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name

-- | Path to the user configuration file in the personal data directory.
configFile :: IO FilePath
configFile = do
  appData <- appDataDir
  return $ combine appData "config"

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
  else error $ "Unknown config option: " ++ s ++ "." ++ o

-- | An association list corresponding to a section. Fails if no such section.
getItems :: CP -> CF.SectionSpec -> [(String, String)]
getItems (CP conf) s =
  if CF.has_section conf s
  then forceEither $ CF.items conf s
  else error $ "Unknown config section: " ++ s

-- | Looks up a file path in the config file and makes it absolute.
-- If the game's configuration directory exists,
-- the file path is appended to it; otherwise, it's appended
-- to the current directory.
getFile :: CP -> CF.SectionSpec -> CF.OptionSpec -> IO FilePath
getFile conf s o = do
  current <- getCurrentDirectory
  appData <- appDataDir
  let path    = get conf s o
      appPath = combine appData path
      curPath = combine current path
  b <- doesDirectoryExist appData
  return $ if b then appPath else curPath

-- | Dumps the current configuration to a file.
dump :: FilePath -> CP -> IO ()
dump fn (CP conf) = do
  current <- getCurrentDirectory
  let path  = combine current fn
      sdump = CF.to_string conf
  writeFile path sdump

-- | Simplified setting of an option in a given section. Overwriting forbidden.
set :: CP -> CF.SectionSpec -> CF.OptionSpec -> String -> CP
set (CP conf) s o v =
  if CF.has_option conf s o
  then error $ "Overwritten config option: " ++ s ++ "." ++ o
  else CP $ forceEither $ CF.set conf s o v

-- | Gets a random generator from the config or,
-- if not present, generates one and updates the config with it.
getSetGen :: CP  -- ^ config
          -> String     -- ^ name of the generator
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
