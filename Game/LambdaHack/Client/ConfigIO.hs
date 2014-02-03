-- | Personal game configuration file support.
module Game.LambdaHack.Client.ConfigIO
  ( CP, mkConfig, get, getOption, set, getItems
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.ConfigFile as CF
import System.Directory

overrideCP :: CP -> FilePath -> IO CP
overrideCP cp@(CP defCF) cfile = do
  cpExists <- doesFileExist cfile
  if not cpExists
    then return cp
    else do
      c <- CF.readfile defCF cfile
      return $! toCP $ assert `forceEither` c

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

-- | Simplified setting of an option in a given section. Overwriting forbidden.
set :: CP -> CF.SectionSpec -> CF.OptionSpec -> String -> CP
set (CP conf) s o v =
  if CF.has_option conf s o
  then assert `failure`"overwritten config option" `twith` (s, o)
  else CP $ assert `forceEither` CF.set conf s o v

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
