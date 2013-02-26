-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUI(..)
  ) where

import Game.LambdaHack.Actor
import Game.LambdaHack.CmdAtomic

-- | Abstract syntax of client commands that don't use the UI.
data CmdCli =
    CmdAtomicCli CmdAtomic
  | CmdHandleAICli ActorId
  deriving Show

-- | Abstract syntax of client commands that use the UI.
data CmdUI =
    CmdAtomicUI CmdAtomic
  | DescAtomicUI DescAtomic
  | CmdHandleHumanUI ActorId
  deriving Show
