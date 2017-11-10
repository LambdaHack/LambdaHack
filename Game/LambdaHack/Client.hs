-- | Semantics of responses that are sent from server to clients
-- and semantics of human commands in terms of requests to be sent
-- to the server.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( -- * Re-exported from "Game.LambdaHack.Client.LoopM"
    loopCli
    -- * Re-exported from "Game.LambdaHack.Client.Response"
  , Response (..)
    -- * Re-exported from "Game.LambdaHack.Client.Request"
  , RequestAI, ReqAI(..), RequestUI, ReqUI(..)
  , RequestAnyAbility(..), RequestTimed(..)
    -- * Re-exported from "Game.LambdaHack.Client.ClientOptions"
  , ClientOptions, defClientOptions, sbenchmark
    -- * Re-exported from "Game.LambdaHack.Client.UI"
  , KeyKind, SessionUI, emptySessionUI
  , UIOptions, applyUIOptions, uCmdline, mkUIOptions
  ) where

import Prelude ()

import Game.LambdaHack.Client.ClientOptions
import Game.LambdaHack.Client.LoopM
import Game.LambdaHack.Client.Request
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.UI
