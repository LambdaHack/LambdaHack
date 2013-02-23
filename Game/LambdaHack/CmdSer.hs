{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract syntax of server commands.
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
    DieSer ActorId
  | MoveSer ActorId Vector
  | RunSer ActorId Vector
  | WaitSer ActorId
  | PickupSer ActorId ItemId Int InvChar
  | DropSer ActorId ItemId
  | ProjectSer ActorId Point Int ItemId Container
  | ApplySer ActorId ItemId Container
  | TriggerSer ActorId Point
  | SetPathSer ActorId [Vector]
  | GameRestartSer
  | GameExitSer
  | GameSaveSer
  | CfgDumpSer
  deriving (Show, Typeable)

timedCmdSer :: CmdSer -> Bool
timedCmdSer cmd = case cmd of
  DieSer{} -> False
  SetPathSer{} -> False
  GameRestartSer -> False
  GameExitSer -> False
  GameSaveSer -> False
  CfgDumpSer -> False
  _ -> True

-- | The actor performing the command, if still alive afterwards.
aidCmdSer :: CmdSer -> Maybe ActorId
aidCmdSer cmd = case cmd of
  DieSer _ -> Nothing
  MoveSer aid _ -> Just aid
  RunSer aid _ -> Just aid
  WaitSer aid -> Just aid
  PickupSer aid _ _ _ -> Just aid
  DropSer aid _ -> Just aid
  ProjectSer aid _ _ _ _ -> Just aid
  ApplySer aid _ _ -> Just aid
  TriggerSer aid _ -> Just aid
  SetPathSer aid _ -> Just aid
  GameRestartSer -> Nothing
  GameExitSer -> Nothing
  GameSaveSer -> Nothing
  CfgDumpSer -> Nothing
