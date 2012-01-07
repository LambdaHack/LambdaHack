-- | The default configurations file included via CPP as a Haskell string.
{-# LANGUAGE CPP, QuasiQuotes #-}
module ConfigDefault ( configDefault ) where

import Multiline

-- Consider code.haskell.org/~dons/code/compiled-constants (dead link, BTW?)
-- as soon as the config file grows very big.

-- | The string containing the default configuration
-- included from file config.default.
-- Warning: cabal does not detect that the default config is changed,
-- so touching this file is needed to reinclude config and recompile.
configDefault :: String
configDefault = [multiline|

#include "../config.default"

|]
