-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUpdateCli(..)
  , CmdUI(..), CmdUpdateUI(..)
  ) where

import Game.LambdaHack.Actor
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Faction
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
  | ShowMsgCli Msg
  | RememberCli Level LevelId ActorDict ItemDict FactionDict
  | RememberPerCli Perception Level LevelId ActorDict ItemDict FactionDict
  | RestartCli FactionPers State
  | ContinueSavedCli FactionPers
  | GameSaveBkpCli
  | GameDisconnectCli
  deriving Show

data CmdUpdateUI =
    CmdAtomicUI CmdAtomic
  | DescAtomicUI DescAtomic
  | DisplayPushUI
  | DisplayDelayUI
  | MoreFullUI Msg
  | MoreBWUI Msg
  | FlushFramesUI
  deriving Show
