{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUpdateCli(..), CmdQueryCli(..)
  , CmdUI(..), CmdUpdateUI(..), CmdQueryUI(..)
  ) where

import qualified Data.EnumSet as ES
import Data.Typeable

import Game.LambdaHack.Actor
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
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
  | DiscoverCli (Kind.Id ItemKind) Item
  | RemCli (ES.EnumSet Point) Level LevelId
  | RememberCli Level LevelId ActorDict ItemDict FactionDict
  | RememberPerCli Perception Level LevelId ActorDict ItemDict FactionDict
  | SwitchLevelCli ActorId LevelId Actor ItemBag
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
  SelectLeaderCli :: ActorId -> CmdQueryCli Bool
  NullReportCli :: CmdQueryCli Bool
  HandleAI :: ActorId -> CmdQueryCli CmdSer
  IsRunningCli :: CmdQueryCli Bool

deriving instance Show (CmdQueryCli a)

data CmdQueryUI a where
  ShowSlidesCli :: Slideshow -> CmdQueryUI Bool
  CarryOnCli :: CmdQueryUI Bool
  ConfirmShowItemsCli :: Msg -> ItemBag -> ItemInv -> CmdQueryUI Bool
  ConfirmShowItemsFloorCli :: Msg -> ItemBag -> CmdQueryUI Bool
  ConfirmYesNoCli :: Msg -> CmdQueryUI Bool
  ConfirmMoreBWCli :: Msg -> CmdQueryUI Bool
  ConfirmMoreFullCli::  Msg -> CmdQueryUI Bool
  HandleHumanCli :: CmdQueryUI [CmdSer]
  FlushFramesCli :: FactionId -> CmdQueryUI Bool

deriving instance Show (CmdQueryUI a)
