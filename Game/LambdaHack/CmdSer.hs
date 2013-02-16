{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract syntax of server commands.
module Game.LambdaHack.CmdSer
  ( CmdSer(..), timedCmdSer
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
  | ClearPathSer ActorId
  | SetPathSer ActorId Vector [Vector]
  | GameRestartSer
  | LeaderSer ActorId
  | GameExitSer
  | GameSaveSer
  | CfgDumpSer
  deriving (Show, Typeable)

timedCmdSer :: CmdSer -> Bool
timedCmdSer cmd = case cmd of
  DieSer{} -> False
  ClearPathSer{} -> False
  SetPathSer{} -> False
  GameRestartSer -> False
  LeaderSer{} -> False
  GameExitSer -> False
  GameSaveSer -> False
  CfgDumpSer -> False
  _ -> True
