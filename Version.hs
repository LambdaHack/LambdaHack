module Version where

import Data.Version

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Display

version :: String
version = showVersion Self.version ++ " (" ++ displayId ++ " frontend)"

