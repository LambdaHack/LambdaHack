module Config where

import System.Directory
import Data.ConfigFile
import Data.Either.Utils
import Control.Monad.Error

-- | Path to the main configuration file.
file :: String
file = "LambdaHack.config"

-- | The configuration read from the main configuration file.
-- If no such file, generate empty configuration.
config :: MonadError CPError m => IO (m ConfigParser)
config =
  do
    b <- doesFileExist file
    if b
      then readfile emptyCP file
      else return $ return emptyCP

-- | A simplified access to a string option in a given section,
-- with simple error reporting (no error is caught and hidden).
-- If there is no config file or no such option, gives Nothing.
-- Note: with a type variable in place of String, it fails becausee of
-- overlapping instances. I think that's just a restriction of
-- ad-hoc polymorphism and it can't be worked around.
getString :: SectionSpec -> OptionSpec -> IO (Maybe String)
getString s o =
  do
    cfg <- config
    let cfgForced = forceEither cfg
    if has_option cfgForced s o
      then let val = get cfgForced s o
               valForced = forceEither val
           in  return (Just valForced)
      else return Nothing
