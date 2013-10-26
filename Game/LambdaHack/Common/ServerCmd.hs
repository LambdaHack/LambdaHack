{-# LANGUAGE OverloadedStrings #-}
-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.ServerCmd
  ( CmdSer(..), CmdSerTakeTime(..), aidCmdSer, aidCmdSerTakeTime
  , FailureSer(..), showFailureSer
  ) where

import Data.Text (Text)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector

-- | Abstract syntax of server commands.
data CmdSer =
    TakeTimeSer CmdSerTakeTime
  | GameRestartSer ActorId Text
  | GameExitSer ActorId
  | GameSaveSer ActorId
  | CfgDumpSer ActorId
  deriving (Show)

data CmdSerTakeTime =
    MoveSer ActorId Vector
  | DisplaceSer ActorId Vector
  | AlterSer ActorId Vector
  | WaitSer ActorId
  | PickupSer ActorId ItemId Int InvChar
  | DropSer ActorId ItemId
  | ProjectSer ActorId Point Int ItemId Container
  | ApplySer ActorId ItemId Container
  | TriggerSer ActorId
  | SetPathSer ActorId [Vector]
  deriving (Show)

-- | The actor that start performing the command (may be dead, after
-- the command is performed).
aidCmdSer :: CmdSer -> ActorId
aidCmdSer cmd = case cmd of
  TakeTimeSer cmd2 -> aidCmdSerTakeTime cmd2
  GameRestartSer aid _ -> aid
  GameExitSer aid -> aid
  GameSaveSer aid -> aid
  CfgDumpSer aid -> aid

aidCmdSerTakeTime :: CmdSerTakeTime -> ActorId
aidCmdSerTakeTime cmd = case cmd of
  MoveSer aid _ -> aid
  DisplaceSer aid _ -> aid
  AlterSer aid _ -> aid
  WaitSer aid -> aid
  PickupSer aid _ _ _ -> aid
  DropSer aid _ -> aid
  ProjectSer aid _ _ _ _ -> aid
  ApplySer aid _ _ -> aid
  TriggerSer aid -> aid
  SetPathSer aid _ -> aid

data FailureSer =
    MoveNothing
  | RunNothing
  | RunDisplaceAccess
  | ProjectAimOnself
  | ProjectBlockTerrain
  | ProjectBlockActor
  | ProjectBlockFoes
  | AlterBlockActor
  | AlterBlockItem
  | AlterNothing
  | TriggerNothing

showFailureSer :: FailureSer -> Msg
showFailureSer failureSer = case failureSer of
  MoveNothing -> "wasting time on moving into obstacle"
  RunNothing -> "switching places with non-existent actor"
  RunDisplaceAccess -> "switching places without access"
  ProjectAimOnself -> "cannot aim at oneself"
  ProjectBlockTerrain -> "aiming obstructed by terrain"
  ProjectBlockActor -> "aiming blocked by an actor"
  ProjectBlockFoes -> "aiming interrupted by foes"
  AlterBlockActor -> "blocked by an actor"
  AlterBlockItem -> "jammed by an item"
  AlterNothing -> "wasting time on altering nothing"
  TriggerNothing -> "wasting time on triggering nothing"
