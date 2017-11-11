-- | Semantics of requests that are sent by clients to the server,
-- in terms of game state changes and responses to be sent to the clients.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server
  ( -- * Re-exported from "Game.LambdaHack.Server.LoopM"
    loopSer
    -- * Re-exported from "Game.LambdaHack.Server.ProtocolM"
  , ChanServer (..)
    -- * Re-exported from "Game.LambdaHack.Server.Commandline"
  , serverOptionsPI
    -- * Re-exported from "Game.LambdaHack.Server.ServerOptions"
  , ServerOptions(..)
  ) where

import Prelude ()

import Game.LambdaHack.Server.Commandline (serverOptionsPI)
import Game.LambdaHack.Server.LoopM (loopSer)
import Game.LambdaHack.Server.ProtocolM
import Game.LambdaHack.Server.ServerOptions
