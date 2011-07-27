module Config
 (CP, defaultCP, config, getOption, getItems, get, getFile, dump) where

import System.Directory
import System.FilePath
import System.IO
import Control.Monad.Error

import qualified Data.ConfigFile as CF
import Data.Either.Utils
import Data.Maybe
import qualified Data.Binary as Binary

import qualified ConfigDefault

newtype CP = CP CF.ConfigParser

instance Binary.Binary CP where
  put (CP config) = Binary.put $ CF.to_string config
  get = do
    string <- Binary.get
    -- use config in case savegame is from older version and lacks some options
    let c = CF.readstring defCF string
    return $ toCP $ forceEither c

instance Show CP where
  show (CP config) = show $ CF.to_string config

-- | Switches all names to case sensitive (unlike by default in ConfigFile).
toSensitive :: CF.ConfigParser -> CF.ConfigParser
toSensitive cp = cp {CF.optionxform = id}

-- | The default configuration taken from the default configuration file
-- included via CPP in ConfigDefault.hs.
defCF :: CF.ConfigParser
defCF  =
  let c = CF.readstring CF.emptyCP ConfigDefault.configDefault
  in  toSensitive $ forceEither c

toCP :: CF.ConfigParser -> CP
toCP cp = CP $ toSensitive cp

defaultCP :: CP
defaultCP = toCP defCF

-- | Path to the user configuration file.
file :: IO FilePath
file =
  do
    appData <- getAppUserDataDirectory "LambdaHack"
    return $ combine appData "LambdaHack.config"

-- | The configuration read from the user configuration file.
-- The default configuration file provides underlying defaults
-- in case some options, or the whole file, are missing.
config :: IO CP
config =
  -- evaluate, to catch config errors ASAP
  defCF `seq`
  do
    f <- file
    b <- doesFileExist f
    if not b
      then return $ toCP $ defCF
      else do
        c <- CF.readfile defCF f
        return $ toCP $ forceEither c

-- | A simplified access to an option in a given section,
-- with simple error reporting (no error is caught and hidden).
-- If there is no config file or no such option, gives Nothing.
getOption :: CF.Get_C a => CP -> CF.SectionSpec -> CF.OptionSpec -> Maybe a
getOption (CP config) s o =
  if CF.has_option config s o
  then Just $ forceEither $ CF.get config s o
  else Nothing

-- | A simplified access to an option in a given section.
get :: CF.Get_C a => CP -> CF.SectionSpec -> CF.OptionSpec -> a
get (CP config) s o =
  if CF.has_option config s o
  then forceEither $ CF.get config s o
  else error $ "unknown config option: " ++ s ++ "." ++ o

-- | An association list corresponding to a section.
getItems :: CP -> CF.SectionSpec -> [(String, String)]
getItems (CP config) s =
  if CF.has_section config s
  then forceEither $ CF.items config s
  else error $ "unknown config section: " ++ s

-- | Looks up a file path in the config file and makes it absolute.
-- If the game's configuration directory exists,
-- the path is appended to it; otherwise, it's appended
-- to the current directory.
getFile :: CP -> CF.SectionSpec -> CF.OptionSpec -> IO FilePath
getFile config s o =
  do
    current <- getCurrentDirectory
    appData <- getAppUserDataDirectory "LambdaHack"
    let path    = get config s o
        appPath = combine appData path
        curPath = combine current path
    b <- doesDirectoryExist appData
    return $ if b then appPath else curPath

dump :: FilePath -> CP -> IO ()
dump fn (CP config) =
  do
    current <- getCurrentDirectory
    let path = combine current fn
        dump = CF.to_string config
    writeFile path dump
