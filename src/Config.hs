module Config where

import System.Directory
import System.FilePath
import Control.Monad.Error

import Data.ConfigFile
import Data.Either.Utils
import Data.Maybe
import qualified Data.Binary as B

newtype CP = CP ConfigParser

instance B.Binary CP where
  put (CP config) = B.put $ to_string config
  get = do
    string <- B.get
    let parsed = readstring emptyCP string
    return $ CP $ forceEither $ parsed

instance Show CP where
  show (CP config) = show $ to_string config

-- | Underscore in the name to avoid name clash.
empty_CP :: CP
empty_CP = CP emptyCP

-- | Path to the main configuration file.
file :: IO String
file =
  do
    appData <- getAppUserDataDirectory "LambdaHack"
    return $ combine appData "LambdaHack.config"

-- | The configuration read from the main configuration file.
-- If no such file, generate empty configuration.
config :: IO CP
config =
  do
    f <- file
    b <- doesFileExist f
    if not b
      then return $ empty_CP
      else do
        c <- readfile emptyCP f
        return $ CP (forceEither c)

-- | A simplified access to an option in a given section,
-- with simple error reporting (no error is caught and hidden).
-- If there is no config file or no such option, gives Nothing.
getOption :: Get_C a => CP -> SectionSpec -> OptionSpec ->
             Maybe a
getOption (CP config) s o =
  do
    if has_option config s o
      then let val = get config s o
               valForced = forceEither val
           in  Just valForced
      else Nothing

-- | Looks up a file path in the config file, faling back to the default path.
-- The path from the config file is taken relative to the home directory
-- and the default is taken relative to the current directory. In any case,
-- the returned path is absolute.
getFile :: CP -> FilePath -> SectionSpec -> OptionSpec -> IO FilePath
getFile config dflt s o =
  do
    current <- getCurrentDirectory
    appData <- getAppUserDataDirectory "LambdaHack"
    let path = getOption config s o
    return $ maybe (combine current dflt) (combine appData) path
