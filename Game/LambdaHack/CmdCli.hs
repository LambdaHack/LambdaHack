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

-- | Abstract syntax of client commands that use the UI.
data CmdUI where
  CmdUpdateUI :: CmdUpdateUI -> CmdUI
  CmdQueryUI :: forall a. Typeable a => CmdQueryUI a -> CmdUI

deriving instance Show CmdUI

data CmdUpdateCli =
    CmdAtomicCli CmdAtomic
  | ShowMsgCli Msg
  | RememberCli Level LevelId ActorDict ItemDict FactionDict
  | RememberPerCli Perception Level LevelId ActorDict ItemDict FactionDict
  | RestartCli FactionPers State
  | ContinueSavedCli FactionPers
  | GameSaveBkpCli Bool
  | GameDisconnectCli Bool
  deriving Show

data CmdQueryCli a where
  NullReportCli :: CmdQueryCli Bool
  HandleAICli :: ActorId -> CmdQueryCli CmdSer
  IsRunningCli :: CmdQueryCli Bool

deriving instance Show (CmdQueryCli a)

data CmdUpdateUI =
    CmdAtomicUI CmdAtomic
  | DescAtomicUI DescAtomic
  | DisplayPushUI
  | DisplayDelayUI
  | MoreFullUI Msg
  | MoreBWUI Msg
  deriving Show

data CmdQueryUI a where
  ShowSlidesUI :: Slideshow -> CmdQueryUI Bool
  ConfirmMoreBWUI :: Msg -> CmdQueryUI Bool
  HandleHumanUI :: CmdQueryUI [CmdSer]
  FlushFramesUI :: FactionId -> CmdQueryUI Bool

deriving instance Show (CmdQueryUI a)
