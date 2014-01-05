-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.ServerCmd
  ( CmdSer(..), CmdTakeTimeSer(..), aidCmdSer, aidCmdTakeTimeSer
  , FailureSer(..), showFailureSer
  ) where

import Data.Text (Text)

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector

-- | Abstract syntax of server commands.
data CmdSer =
    CmdTakeTimeSer !CmdTakeTimeSer
  | GameRestartSer !ActorId !Text
  | GameExitSer !ActorId
  | GameSaveSer !ActorId
  | GameDifficultySer !ActorId !Int
  deriving (Show, Eq)

data CmdTakeTimeSer =
    MoveSer !ActorId !Vector
  | MeleeSer !ActorId !ActorId
  | DisplaceSer !ActorId !ActorId
  | AlterSer !ActorId !Point !(Maybe F.Feature)
  | WaitSer !ActorId
  | PickupSer !ActorId !ItemId !Int
  | DropSer !ActorId !ItemId !Int
  | ProjectSer !ActorId !Point !Int !ItemId !Container
  | ApplySer !ActorId !ItemId !Container
  | TriggerSer !ActorId !(Maybe F.Feature)
  | SetPathSer !ActorId
  deriving (Show, Eq)

-- | The actor that starts performing the command (may be dead, after
-- the command is performed).
aidCmdSer :: CmdSer -> ActorId
aidCmdSer cmd = case cmd of
  CmdTakeTimeSer cmd2 -> aidCmdTakeTimeSer cmd2
  GameRestartSer aid _ -> aid
  GameExitSer aid -> aid
  GameSaveSer aid -> aid
  GameDifficultySer aid _ -> aid

aidCmdTakeTimeSer :: CmdTakeTimeSer -> ActorId
aidCmdTakeTimeSer cmd = case cmd of
  MoveSer aid _ -> aid
  MeleeSer aid _ -> aid
  DisplaceSer aid _ -> aid
  AlterSer aid _ _ -> aid
  WaitSer aid -> aid
  PickupSer aid _ _ -> aid
  DropSer aid _ _ -> aid
  ProjectSer aid _ _ _ _ -> aid
  ApplySer aid _ _ -> aid
  TriggerSer aid _ -> aid
  SetPathSer aid -> aid

data FailureSer =
    MoveNothing
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
  | TriggerNothing

showFailureSer :: FailureSer -> Msg
showFailureSer failureSer = case failureSer of
  MoveNothing -> "wasting time on moving into obstacle"
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
  TriggerNothing -> "wasting time on triggering nothing"
