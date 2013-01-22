{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdUpdateCli, cmdQueryCli
  , loopCli2, executorCli, exeFrontend
  ) where

import Control.Monad

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.SemAction
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Client.State
import Game.LambdaHack.Msg
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.LocalAction

cmdUpdateCli :: MonadClientUI m => CmdUpdateCli -> m ()
cmdUpdateCli cmd = case cmd of
  PickupCli aid i ni -> pickupCli aid i ni
  ApplyCli actor verb item -> applyCli actor verb item
  ShowItemsCli discoS msg items -> showItemsCli discoS msg items
  ShowMsgCli msg -> msgAdd msg
  AnimateDeathCli aid -> animateDeathCli aid
  InvalidateArenaCli lid -> void $ invalidateArenaCli lid
  DiscoverCli ik i -> discoverCli ik i
  RememberCli arena vis lvl -> rememberCli arena vis lvl
  RememberPerCli arena per lvl faction -> rememberPerCli arena per lvl faction
  SwitchLevelCli aid arena pbody items -> switchLevelCli aid arena pbody items
  EffectCli msg poss deltaHP block -> effectCli msg poss deltaHP block
  ProjectCli spos source consumed -> projectCli spos source consumed
  ShowAttackCli source target verb stack say ->
    showAttackCli source target verb stack say
  AnimateBlockCli source target verb -> animateBlockCli source target verb
  DisplaceCli source target -> displaceCli source target
  DisplayPushCli -> displayPush
  DisplayDelayCli -> displayFramesPush [Nothing]
  MoreBWCli msg -> do
    void $ displayMore ColorBW msg
    recordHistory
  MoreFullCli msg -> do
    void $ displayMore ColorFull msg
    recordHistory
  RestartCli sper locRaw -> restartCli sper locRaw
  ContinueSavedCli sper -> modifyClient $ \cli -> cli {sper}
  GameSaveCli toBkp -> clientGameSave toBkp

cmdQueryCli :: MonadClientUI m => CmdQueryCli a -> m a
cmdQueryCli cmd = case cmd of
  ShowSlidesCli slides -> getManyConfirms [] slides
  CarryOnCli -> carryOnCli
  ConfirmShowItemsCli discoS msg items -> do
    io <- itemOverlay discoS True items
    slides <- overlayToSlideshow msg io
    getManyConfirms [] slides
  SelectLeaderCli aid lid -> selectLeader aid lid
  ConfirmYesNoCli msg -> do
    go <- displayYesNo msg
    recordHistory  -- Prevent repeating the ending msgs.
    return go
  ConfirmMoreBWCli msg -> do
    go <- displayMore ColorBW msg
    recordHistory  -- Prevent repeating the ending msgs.
    return go
  ConfirmMoreFullCli msg -> do
    go <- displayMore ColorFull msg
    recordHistory  -- Prevent repeating the ending msgs.
    return go
  NullReportCli -> do
    StateClient{sreport} <- getClient
    return $! nullReport sreport
  SetArenaLeaderCli arena actor -> setArenaLeaderCli arena actor
  HandleHumanCli leader -> handleHuman leader
  HandleAI actor -> handleAI actor
