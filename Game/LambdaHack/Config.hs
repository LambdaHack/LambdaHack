-- | Personal game configuration file support.
module Game.LambdaHack.Config
  ( CP(..), toCP, forceEither, getOption, get, getItems
  ) where

import qualified Data.ConfigFile as CF
import qualified Data.Binary as Binary

import Game.LambdaHack.Utils.Assert

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
