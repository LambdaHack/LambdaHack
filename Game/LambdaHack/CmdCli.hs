{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUpdateCli(..), CmdQueryCli(..)
  ) where

import qualified Data.IntSet as IS
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

-- | Abstract syntax of client commands.
data CmdCli where
  CmdUpdateCli :: CmdUpdateCli -> CmdCli
  CmdQueryCli :: forall a. Typeable a => CmdQueryCli a -> CmdCli

deriving instance Show CmdCli

data CmdUpdateCli =
    PickupCli ActorId Item Item
  | ApplyCli ActorId MU.Part Item
  | ShowMsgCli Msg
  | ShowItemsCli Discoveries Msg [Item]
  | AnimateDeathCli ActorId
  | InvalidateArenaCli LevelId
  | DiscoverCli (Kind.Id ItemKind) Item
  | RememberCli LevelId IS.IntSet Level  -- TODO: Level is an overkill
  | RememberPerCli LevelId Perception Level FactionDict
  | SwitchLevelCli ActorId LevelId Actor [Item]
  | EffectCli Msg (Point, Point) Int Bool
  | ProjectCli Point ActorId Item
  | ShowAttackCli ActorId ActorId MU.Part Item Bool
  | AnimateBlockCli ActorId ActorId MU.Part
  | DisplaceCli ActorId ActorId
  | DisplayPushCli
  | DisplayDelayCli
  | MoreFullCli Msg
  | MoreBWCli Msg
  | RestartCli FactionPers State
  | ContinueSavedCli FactionPers
  | GameSaveCli Bool
  deriving Show

data CmdQueryCli a where
  ShowSlidesCli :: Slideshow -> CmdQueryCli Bool
  CarryOnCli :: CmdQueryCli Bool
  ConfirmShowItemsCli :: Discoveries -> Msg -> [Item] -> CmdQueryCli Bool
  SelectLeaderCli :: ActorId -> LevelId -> CmdQueryCli Bool
  ConfirmYesNoCli :: Msg -> CmdQueryCli Bool
  ConfirmMoreBWCli :: Msg -> CmdQueryCli Bool
  ConfirmMoreFullCli::  Msg -> CmdQueryCli Bool
  NullReportCli :: CmdQueryCli Bool
  SetArenaLeaderCli :: LevelId -> ActorId -> CmdQueryCli ActorId
  HandleHumanCli :: ActorId -> CmdQueryCli (CmdSer, Maybe ActorId, LevelId)
  HandleAI :: ActorId -> CmdQueryCli CmdSer

deriving instance Show (CmdQueryCli a)
