{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract syntax of server commands.
-- See https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture.
module Game.LambdaHack.CmdSer
  ( CmdSer(..), timedCmdSer, aidCmdSer
  ) where

import Data.Typeable

import Game.LambdaHack.Actor
import Game.LambdaHack.Item
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import Game.LambdaHack.Vector

-- | Abstract syntax of server commands.
data CmdSer =
    MoveSer ActorId Vector
  | RunSer ActorId Vector
  | WaitSer ActorId
  | PickupSer ActorId ItemId Int InvChar
  | DropSer ActorId ItemId
  | ProjectSer ActorId Point Int ItemId Container
  | ApplySer ActorId ItemId Container
  | TriggerSer ActorId Point
  | SetPathSer ActorId [Vector]
  | GameRestartSer ActorId
  | GameExitSer ActorId
  | GameSaveSer ActorId
  | CfgDumpSer ActorId
  deriving (Show, Typeable)

timedCmdSer :: CmdSer -> Bool
timedCmdSer cmd = case cmd of
  SetPathSer _ path -> length path > 1
  GameRestartSer{} -> False
  GameExitSer{} -> False
  GameSaveSer{} -> False
  CfgDumpSer{} -> False
  _ -> True

-- | The actor performing the command, if still alive afterwards.
aidCmdSer :: CmdSer -> ActorId
aidCmdSer cmd = case cmd of
  MoveSer aid _ -> aid
  RunSer aid _ -> aid
  WaitSer aid -> aid
  PickupSer aid _ _ _ -> aid
  DropSer aid _ -> aid
  ProjectSer aid _ _ _ _ -> aid
  ApplySer aid _ _ -> aid
  TriggerSer aid _ -> aid
  SetPathSer aid _ -> aid
  GameRestartSer aid -> aid
  GameExitSer aid -> aid
  GameSaveSer aid -> aid
  CfgDumpSer aid -> aid
