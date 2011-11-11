module Game.LambdaHack.Version where

import Data.Version

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import qualified Game.LambdaHack.Display as Display

version :: String
version = showVersion Self.version ++ " (" ++ Display.displayId ++ " frontend)"
