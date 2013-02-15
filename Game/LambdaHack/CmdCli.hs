{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUpdateCli(..), CmdQueryCli(..)
  , CmdUI(..), CmdUpdateUI(..), CmdQueryUI(..)
  ) where

import Data.Typeable

import Game.LambdaHack.Actor
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State

-- | Abstract syntax of client commands that don't use the UI.
data CmdCli where
  CmdUpdateCli :: CmdUpdateCli -> CmdCli
  CmdQueryCli :: forall a. Typeable a => CmdQueryCli a -> CmdCli

deriving instance Show CmdCli

-- | Abstract syntax of all client commands.
data CmdUI where
  CmdUpdateUI :: CmdUpdateUI -> CmdUI
  CmdQueryUI :: forall a. Typeable a => CmdQueryUI a -> CmdUI

deriving instance Show CmdUI

data CmdUpdateCli =
    ShowMsgCli Msg
  | RememberCli Level LevelId ActorDict ItemDict FactionDict
  | RememberPerCli Perception Level LevelId ActorDict ItemDict FactionDict
  | RestartCli FactionPers State
  | ContinueSavedCli FactionPers
  | GameSaveBkpCli Bool
  | GameDisconnectCli Bool
  | AtomicSeen Atomic
  deriving Show

data CmdUpdateUI =
    DisplayPushCli
  | DisplayDelayCli
  | MoreFullCli Msg
  | MoreBWCli Msg
  | AtomicSeenUI Atomic
  deriving Show

data CmdQueryCli a where
  NullReportCli :: CmdQueryCli Bool
  HandleAI :: ActorId -> CmdQueryCli CmdSer
  IsRunningCli :: CmdQueryCli Bool

deriving instance Show (CmdQueryCli a)

data CmdQueryUI a where
  ShowSlidesCli :: Slideshow -> CmdQueryUI Bool
  ConfirmMoreBWCli :: Msg -> CmdQueryUI Bool
  HandleHumanCli :: CmdQueryUI [CmdSer]
  FlushFramesCli :: FactionId -> CmdQueryUI Bool

deriving instance Show (CmdQueryUI a)
