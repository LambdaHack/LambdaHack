{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdUpdateCli, cmdUpdateUI, cmdQueryCli, cmdQueryUI
  , loopCli2, loopCli4, executorCli, exeFrontend
  , MonadClientChan, MonadClientUI
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.CmdCliSem
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.LocalAction
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Faction
import Game.LambdaHack.Msg
import Game.LambdaHack.State

cmdUpdateCli :: MonadClient m => CmdUpdateCli -> m ()
cmdUpdateCli cmd = case cmd of
  PickupCli aid iid k l -> pickupCli aid iid k l
  ApplyCli actor verb item -> applyCli actor verb item
  ShowMsgCli msg -> msgAdd msg
  InvalidateArenaCli lid -> void $ invalidateArenaCli lid
  DiscoverCli ik i -> discoverCli ik i
  RememberCli arena vis lvl -> rememberCli arena vis lvl
  RememberPerCli arena per lvl itemD faction -> rememberPerCli arena per lvl itemD faction
  SwitchLevelCli _aid _arena _pbody _items -> undefined  -- switchLevelCli aid arena pbody items
  ProjectCli spos source item -> projectCli spos source item
  ShowAttackCli source target verb stack say ->
    showAttackCli source target verb stack say
  RestartCli sper locRaw -> restartCli sper locRaw
  ContinueSavedCli sper -> modifyClient $ \cli -> cli {sper}
  GameSaveBkpCli isAI -> clientGameSave True isAI
  GameDisconnectCli isAI -> clientDisconnect isAI

cmdUpdateUI :: MonadClientUI m => CmdUpdateUI -> m ()
cmdUpdateUI cmd = case cmd of
  AnimateDeathCli aid -> animateDeathCli aid
  EffectCli msg poss deltaHP block -> effectCli msg poss deltaHP block
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

cmdQueryCli :: MonadClient m => CmdQueryCli a -> m a
cmdQueryCli cmd = case cmd of
  SelectLeaderCli aid lid -> selectLeader aid lid
  NullReportCli -> do
    StateClient{sreport} <- getClient
    return $! nullReport sreport
  SetArenaLeaderCli arena actor -> setArenaLeaderCli arena actor
  HandleAI actor -> handleAI actor
  IsRunningCli -> do
    tryWith (\_ -> return False) $ do
      mleader <- getsClient sleader
      leader <- maybe abort return mleader
      srunning <- getsClient srunning
      maybe abort (void . continueRunDir leader) srunning
      return True

cmdQueryUI :: MonadClientUI m => CmdQueryUI a -> m a
cmdQueryUI cmd = case cmd of
  ShowSlidesCli slides -> getManyConfirms [] slides
  CarryOnCli -> carryOnCli
  ConfirmShowItemsCli msg bag inv -> do
    io <- itemOverlay bag inv
    slides <- overlayToSlideshow msg io
    getManyConfirms [] slides
  ConfirmShowItemsFloorCli msg bag -> do
    io <- floorItemOverlay bag
    slides <- overlayToSlideshow msg io
    getManyConfirms [] slides
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
  HandleHumanCli leader -> handleHuman leader
  FlushFramesCli newSide -> do
    srunning <- getsClient srunning
    case srunning of
      Just (_, k) | k > 1 -> return False
      _ -> do
        faction <- getsState sfaction
        let factionName = gname $ faction EM.! newSide
            msg = "Switching to player" <+> factionName <> "."
        void $ displayMore ColorFull msg
        -- Messages shown, so update history and reset current report.
        recordHistory
        return True
