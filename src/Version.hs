module Version where

import Data.Version

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import qualified Display

version :: String
version = showVersion Self.version ++ " (" ++ Display.displayId ++ " frontend)"
