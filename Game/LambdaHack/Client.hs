{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of responses that are sent to clients.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( -- * Re-exported from "Game.LambdaHack.Client.LoopClient"
    loopAI, loopUI
  ) where

import Prelude ()

import Game.LambdaHack.Client.LoopM
