{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUpdateCli(..), CmdQueryCli(..)
  , CmdUI(..), CmdUpdateUI(..), CmdQueryUI(..)
  ) where

import qualified Data.EnumSet as ES
import Data.Typeable
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Actor
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
    PickupCli ActorId Item Int (Maybe Char)
  | ApplyCli ActorId MU.Part Item
  | ShowMsgCli Msg
  | InvalidateArenaCli LevelId
  | DiscoverCli (Kind.Id ItemKind) Item
  | RememberCli LevelId (ES.EnumSet Point) Level  -- TODO: Level is an overkill
  | RememberPerCli LevelId Perception Level FactionDict
  | SwitchLevelCli ActorId LevelId Actor ItemBag
  | ProjectCli Point ActorId Item
  | ShowAttackCli ActorId ActorId MU.Part Item Bool
  | RestartCli FactionPers State
  | ContinueSavedCli FactionPers
  | GameSaveBkpCli Bool
  | GameDisconnectCli Bool
  deriving Show

data CmdUpdateUI =
    ShowItemsCli Discoveries Msg ItemBag
  | AnimateDeathCli ActorId
  | EffectCli Msg (Point, Point) Int Bool
  | AnimateBlockCli ActorId ActorId MU.Part
  | DisplaceCli ActorId ActorId
  | DisplayPushCli
  | DisplayDelayCli
  | MoreFullCli Msg
  | MoreBWCli Msg
  deriving Show

data CmdQueryCli a where
  SelectLeaderCli :: ActorId -> LevelId -> CmdQueryCli Bool
  NullReportCli :: CmdQueryCli Bool
  SetArenaLeaderCli :: LevelId -> ActorId -> CmdQueryCli ActorId
  HandleAI :: ActorId -> CmdQueryCli CmdSer
  IsRunningCli :: CmdQueryCli Bool

deriving instance Show (CmdQueryCli a)

data CmdQueryUI a where
  ShowSlidesCli :: Slideshow -> CmdQueryUI Bool
  CarryOnCli :: CmdQueryUI Bool
  ConfirmShowItemsCli :: Discoveries -> Msg -> ItemBag -> CmdQueryUI Bool
  ConfirmYesNoCli :: Msg -> CmdQueryUI Bool
  ConfirmMoreBWCli :: Msg -> CmdQueryUI Bool
  ConfirmMoreFullCli::  Msg -> CmdQueryUI Bool
  HandleHumanCli :: ActorId -> CmdQueryUI (CmdSer, Maybe ActorId, LevelId)
  FlushFramesCli :: FactionId -> CmdQueryUI Bool

deriving instance Show (CmdQueryUI a)
