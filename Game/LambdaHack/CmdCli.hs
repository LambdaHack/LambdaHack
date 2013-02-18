-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUpdateCli(..)
  , CmdUI(..), CmdUpdateUI(..)
  ) where

import Game.LambdaHack.Actor
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State

-- | Abstract syntax of client commands that don't use the UI.
data CmdCli =
    CmdUpdateCli CmdUpdateCli
  | CmdHandleAICli ActorId
  deriving Show

-- | Abstract syntax of client commands that use the UI.
data CmdUI =
    CmdUpdateUI CmdUpdateUI
  | CmdHandleHumanUI ActorId
  deriving Show

data CmdUpdateCli =
    CmdAtomicCli CmdAtomic
  | RememberPerCli Perception Level LevelId ActorDict ItemDict
  | RestartCli FactionPers State
  | ContinueSavedCli FactionPers
  | GameSaveBkpCli
  | GameDisconnectCli
  deriving Show

data CmdUpdateUI =
    CmdAtomicUI CmdAtomic
  | DescAtomicUI DescAtomic
  | ShowMsgUI Msg
  | DisplayPushUI
  | DisplayDelayUI
  | MoreFullUI Msg
  | MoreBWUI Msg
  | FlushFramesUI
  deriving Show
