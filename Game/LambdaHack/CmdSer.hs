{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Abstract syntax of server commands.
module Game.LambdaHack.CmdSer
  ( CmdSer(..), timedCmdSer
  ) where

import Data.Typeable
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Actor
import Game.LambdaHack.Item
import Game.LambdaHack.Point
import Game.LambdaHack.Vector

-- | Abstract syntax of server commands.
data CmdSer =
    ApplySer ActorId MU.Part Item
  | ProjectSer ActorId Point Int MU.Part Item
  | TriggerSer ActorId Point
  | PickupSer ActorId Item Char
  | DropSer ActorId Item
  | WaitSer ActorId
  | MoveSer ActorId Vector
  | RunSer ActorId Vector
  | GameExitSer
  | GameRestartSer
  | GameSaveSer
  | CfgDumpSer
  | DirToAction ActorId Bool Vector
  | ClearPath ActorId
  | FollowPath ActorId Vector [Vector] Bool
  | DieSer ActorId
  deriving (Show, Typeable)

timedCmdSer :: CmdSer -> Bool
timedCmdSer cmd = case cmd of
  GameExitSer -> False
  GameRestartSer -> False
  GameSaveSer -> False
  CfgDumpSer -> False
  _ -> True
