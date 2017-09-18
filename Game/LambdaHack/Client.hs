-- | Semantics of responses that are sent to clients.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( -- * Re-exported from "Game.LambdaHack.Client.LoopM"
    loopCli
    -- * Re-exported from "Game.LambdaHack.Client.UI"
  , KeyKind, SessionUI, emptySessionUI
  , Config, applyConfigToDebug, configCmdline, mkConfig
  ) where

import Prelude ()

import Game.LambdaHack.Client.LoopM
import Game.LambdaHack.Client.UI
