module Config
 (CP, defaultCP, config, getOption, getDefault, getFile) where

import System.Directory
import System.FilePath
import Control.Monad.Error

import qualified Data.ConfigFile as CF
import Data.Either.Utils
import Data.Maybe
import qualified Data.Binary as B

import qualified ConfigDefault

newtype CP = CP CF.ConfigParser

instance B.Binary CP where
  put (CP config) = B.put $ CF.to_string config
  get = do
    string <- B.get
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

-- | Path to the main configuration file.
file :: IO String
file =
  do
    appData <- getAppUserDataDirectory "LambdaHack"
    return $ combine appData "LambdaHack.config"

-- | The configuration read from the main configuration file.
-- If no such file, use the default configuration.
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
getDefault :: CF.Get_C a => a -> CP -> CF.SectionSpec -> CF.OptionSpec -> a
getDefault dflt (CP config) s o =
  if CF.has_option config s o
    then forceEither $ CF.get config s o
    else dflt

-- | Looks up a file path in the config file, faling back to the default path.
-- The path from the config file is taken relative to the home directory
-- and the default is taken relative to the current directory. In any case,
-- the returned path is absolute.
getFile :: CP -> FilePath -> CF.SectionSpec -> CF.OptionSpec -> IO FilePath
getFile config dflt s o =
  do
    current <- getCurrentDirectory
    appData <- getAppUserDataDirectory "LambdaHack"
    let path = getOption config s o
    return $ maybe (combine current dflt) (combine appData) path
