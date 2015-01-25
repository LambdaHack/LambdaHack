{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of responses that are sent to clients.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( -- * Re-exported from "Game.LambdaHack.Client.LoopClient"
    loopAI, loopUI
    -- * Re-exported from "Game.LambdaHack.Client.UI"
  , srtFrontend
  ) where

import Game.LambdaHack.Client.LoopClient
import Game.LambdaHack.Client.UI
