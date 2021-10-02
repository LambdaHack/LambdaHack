-- | Semantics of responses that are sent from server to clients,
-- in terms of client state transformations,
-- and semantics of human commands and AI moves, in terms of requests
-- to be sent from the client to the server.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( -- * Re-exported from "Game.LambdaHack.Client.LoopM"
    loopCli
    -- * Re-exported from "Game.LambdaHack.Client.Request"
  , RequestAI, ReqAI(..), RequestUI, ReqUI(..), RequestTimed(..)
    -- * Re-exported from "Game.LambdaHack.Client.Response"
  , Response (..)
    -- * Re-exported from "Game.LambdaHack.Client.UI"
  , CCUI
  , UIOptions, applyUIOptions, uOverrideCmdline, mkUIOptions
  ) where

import Prelude ()

import Game.LambdaHack.Client.LoopM
import Game.LambdaHack.Client.Request
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.UI
