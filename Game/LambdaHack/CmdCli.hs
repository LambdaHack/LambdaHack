-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUI(..)
  ) where

import Game.LambdaHack.Actor
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.Perception

-- TODO: move most to atomic
-- | Abstract syntax of client commands that don't use the UI.
data CmdCli =
    CmdAtomicCli CmdAtomic
  | ContinueSavedCli FactionPers
  | GameSaveBkpCli
  | GameDisconnectCli
  | CmdHandleAICli ActorId
  deriving Show

-- TODO: move most to atomic
-- | Abstract syntax of client commands that use the UI.
data CmdUI =
    CmdAtomicUI CmdAtomic
  | DescAtomicUI DescAtomic
  | DisplayPushUI
  | DisplayDelayUI
  | FlushFramesUI
  | CmdHandleHumanUI ActorId
  deriving Show
