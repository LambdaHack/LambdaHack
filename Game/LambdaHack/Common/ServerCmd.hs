-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.ServerCmd
  ( CmdSer(..), aidCmdSer
  ) where

import Data.Text (Text)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector

-- | Abstract syntax of server commands.
data CmdSer =
    MoveSer ActorId Vector
  | ExploreSer ActorId Vector
  | RunSer ActorId Vector
  | WaitSer ActorId
  | PickupSer ActorId ItemId Int InvChar
  | DropSer ActorId ItemId
  | ProjectSer ActorId Point Int ItemId Container
  | ApplySer ActorId ItemId Container
  | TriggerSer ActorId Point
  | SetPathSer ActorId [Vector]
  | GameRestartSer ActorId Text
  | GameExitSer ActorId
  | GameSaveSer ActorId
  | CfgDumpSer ActorId
  deriving (Show)

-- | The actor performing the command, if still alive afterwards.
aidCmdSer :: CmdSer -> ActorId
aidCmdSer cmd = case cmd of
  MoveSer aid _ -> aid
  ExploreSer aid _ -> aid
  RunSer aid _ -> aid
  WaitSer aid -> aid
  PickupSer aid _ _ _ -> aid
  DropSer aid _ -> aid
  ProjectSer aid _ _ _ _ -> aid
  ApplySer aid _ _ -> aid
  TriggerSer aid _ -> aid
  SetPathSer aid _ -> aid
  GameRestartSer aid _ -> aid
  GameExitSer aid -> aid
  GameSaveSer aid -> aid
  CfgDumpSer aid -> aid
