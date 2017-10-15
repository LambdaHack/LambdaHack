-- | Semantics of requests that are sent to the server.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server
  ( -- * Re-exported from "Game.LambdaHack.Server.LoopM"
    loopSer
    -- * Re-exported from "Game.LambdaHack.Server.Commandline"
  , debugModeSerPI
    -- * Re-exported from "Game.LambdaHack.Server.State"
  , DebugModeSer(..)
  ) where

import Prelude ()

import Game.LambdaHack.Server.Commandline (debugModeSerPI)
import Game.LambdaHack.Server.LoopM (loopSer)
import Game.LambdaHack.Server.State (DebugModeSer (..))
