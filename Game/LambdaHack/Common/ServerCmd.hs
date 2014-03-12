-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.ServerCmd
  ( CmdSer(..), CmdTakeTimeSer(..), aidCmdSer, aidCmdTakeTimeSer
  , FailureSer(..), showFailureSer
  ) where

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
  | MoveItemSer !ActorId !ItemId !Int !CStore !CStore
  | ProjectSer !ActorId !Point !Int !ItemId !CStore
  | ApplySer !ActorId !ItemId !CStore
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
  MoveItemSer aid _ _ _ _ -> aid
  ProjectSer aid _ _ _ _ -> aid
  ApplySer aid _ _ -> aid
  TriggerSer aid _ -> aid
  SetTrajectorySer aid -> aid
  PongHackSer _ -> toEnum (-1)  -- needed for --sniffIn

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
  | ItemNothing
  | ItemNotCalm
  | ProjectAimOnself
  | ProjectBlockTerrain
  | ProjectBlockActor
  | ProjectBlind
  | ProjectNotCalm
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
  ItemNothing -> "wasting time on void item manipulation"
  ItemNotCalm -> "you hands are shaking too much to sort through inventory"
  ProjectAimOnself -> "cannot aim at oneself"
  ProjectBlockTerrain -> "aiming obstructed by terrain"
  ProjectBlockActor -> "aiming blocked by an actor"
  ProjectBlind -> "blind actors cannot aim"
  ProjectNotCalm -> "your hands are shaking too much to aim"
  TriggerNothing -> "wasting time on triggering nothing"
