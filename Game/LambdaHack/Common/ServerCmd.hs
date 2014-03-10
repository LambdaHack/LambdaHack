-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.ServerCmd
  ( CmdSer(..), CmdTakeTimeSer(..), aidCmdSer, aidCmdTakeTimeSer
  , FailureSer(..), showFailureSer
  ) where

import Control.Exception.Assert.Sugar
import Data.Text (Text)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.AtomicCmd
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector

-- | Abstract syntax of server commands.
data CmdSer =
    CmdTakeTimeSer !CmdTakeTimeSer
  | GameRestartSer !ActorId !Text !Int ![(Int, Text)]
  | GameExitSer !ActorId !Int
  | GameSaveSer !ActorId
  | AutomateSer !ActorId
  deriving (Show, Eq)

data CmdTakeTimeSer =
    MoveSer !ActorId !Vector
  | MeleeSer !ActorId !ActorId
  | DisplaceSer !ActorId !ActorId
  | AlterSer !ActorId !Point !(Maybe F.Feature)
  | WaitSer !ActorId
  | PickupSer !ActorId !ItemId !Int
  | DropSer !ActorId !ItemId !Int
  | WieldSer !ActorId !ItemId !Int
  | YieldSer !ActorId !ItemId !Int
  | ProjectSer !ActorId !Point !Int !ItemId !Container
  | ApplySer !ActorId !ItemId !Container
  | TriggerSer !ActorId !(Maybe F.Feature)
  | SetTrajectorySer !ActorId
  | PongHackSer [Atomic]
  deriving (Show, Eq)

-- | The actor that starts performing the command (may be dead, after
-- the command is performed).
aidCmdSer :: CmdSer -> ActorId
aidCmdSer cmd = case cmd of
  CmdTakeTimeSer cmd2 -> aidCmdTakeTimeSer cmd2
  GameRestartSer aid _ _ _ -> aid
  GameExitSer aid _ -> aid
  GameSaveSer aid -> aid
  AutomateSer aid -> aid

aidCmdTakeTimeSer :: CmdTakeTimeSer -> ActorId
aidCmdTakeTimeSer cmd = case cmd of
  MoveSer aid _ -> aid
  MeleeSer aid _ -> aid
  DisplaceSer aid _ -> aid
  AlterSer aid _ _ -> aid
  WaitSer aid -> aid
  PickupSer aid _ _ -> aid
  DropSer aid _ _ -> aid
  WieldSer aid _ _ -> aid
  YieldSer aid _ _ -> aid
  ProjectSer aid _ _ _ _ -> aid
  ApplySer aid _ _ -> aid
  TriggerSer aid _ -> aid
  SetTrajectorySer aid -> aid
  PongHackSer _ -> assert `failure` cmd

data FailureSer =
    MoveNothing
  | MeleeSelf
  | MeleeDistant
  | DisplaceDistant
  | DisplaceAccess
  | DisplaceProjectiles
  | AlterDistant
  | AlterBlockActor
  | AlterBlockItem
  | AlterNothing
  | PickupOverfull
  | ProjectAimOnself
  | ProjectBlockTerrain
  | ProjectBlockActor
  | ProjectBlockFoes
  | ProjectBlind
  | TriggerNothing

showFailureSer :: FailureSer -> Msg
showFailureSer failureSer = case failureSer of
  MoveNothing -> "wasting time on moving into obstacle"
  MeleeSelf -> "trying to melee oneself"
  MeleeDistant -> "trying to melee a distant foe"
  DisplaceDistant -> "trying to switch places with a distant actor"
  DisplaceAccess -> "switching places without access"
  DisplaceProjectiles -> "trying to switch places with multiple projectiles"
  AlterDistant -> "trying to alter a distant tile"
  AlterBlockActor -> "blocked by an actor"
  AlterBlockItem -> "jammed by an item"
  AlterNothing -> "wasting time on altering nothing"
  PickupOverfull -> "cannot carry any more"
  ProjectAimOnself -> "cannot aim at oneself"
  ProjectBlockTerrain -> "aiming obstructed by terrain"
  ProjectBlockActor -> "aiming blocked by an actor"
  ProjectBlockFoes -> "aiming interrupted by foes"
  ProjectBlind -> "blind actors cannot aim"
  TriggerNothing -> "wasting time on triggering nothing"
