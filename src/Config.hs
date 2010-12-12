module Config where

import System.Directory
import System.FilePath
import Control.Monad.Error

import Data.ConfigFile
import Data.Either.Utils
import Data.Maybe
import qualified Data.Binary as B

-- | Path to the main configuration file.
file :: IO String
file =
  do
    appData <- getAppUserDataDirectory "LambdaHack"
    return $ combine appData "LambdaHack.config"

-- | The configuration read from the main configuration file.
-- If no such file, generate empty configuration.
config :: MonadError CPError m => IO (m ConfigParser)
config =
  do
    f <- file
    b <- doesFileExist f
    if b
      then readfile emptyCP f
      else return $ return emptyCP

-- | A simplified access to an option in a given section,
-- with simple error reporting (no error is caught and hidden).
-- If there is no config file or no such option, gives Nothing.
getOption :: Get_C a => SectionSpec -> OptionSpec -> IO (Maybe a)
getOption s o =
  do
    cfg <- config
    let cfgForced = forceEither cfg
    if has_option cfgForced s o
      then let val = get cfgForced s o
               valForced = forceEither val
           in  return $ Just valForced
      else return Nothing

-- | Looks up a file path in the config file, faling back to the default path.
-- The path from the config file is taken relative to the home directory
-- and the default is taken relative to the current directory. In any case,
-- the returned path is absolute.
getFile :: FilePath -> SectionSpec -> OptionSpec -> IO FilePath
getFile dflt s o =
  do
    current <- getCurrentDirectory
    appData <- getAppUserDataDirectory "LambdaHack"
    s <- getOption s o
    return $ maybe (combine current dflt) (combine appData) s


instance B.Binary ConfigParser where
  put config = B.put $ to_string config
  get = do
    string <- B.get
    let parsed = readstring emptyCP string
    return $ forceEither $ parsed

instance Show ConfigParser where
  show config = show $ to_string config
