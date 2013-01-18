{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdUpdateCli, cmdQueryCli, loopClient2
  ) where

import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.SemAction
