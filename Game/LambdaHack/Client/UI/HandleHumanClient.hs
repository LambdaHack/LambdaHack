-- | Semantics of human player commands.
module Game.LambdaHack.Client.UI.HandleHumanClient
  ( cmdHumanSem
  ) where

import Control.Applicative
import Data.Monoid

import Game.LambdaHack.Client.UI.HandleHumanGlobalClient
import Game.LambdaHack.Client.UI.HandleHumanLocalClient
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Common.Request

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Some time cosuming commands are enabled in targeting mode, but cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the leader).
cmdHumanSem :: MonadClientUI m => HumanCmd -> m (SlideOrCmd RequestUI)
cmdHumanSem cmd = do
  if noRemoteHumanCmd cmd then do
    -- If in targeting mode, check if the current level is the same
    -- as player level and refuse performing the action otherwise.
    arena <- getArenaUI
    lidV <- viewedLevel
    if (arena /= lidV) then
      failWith $ "command disabled on a remote level, press ESC to switch back"
    else cmdAction cmd
  else cmdAction cmd

-- | Compute the basic action for a command and mark whether it takes time.
cmdAction :: MonadClientUI m => HumanCmd -> m (SlideOrCmd RequestUI)
cmdAction cmd = case cmd of
  -- Global.
  Move v -> fmap anyToUI <$> moveRunHuman False v
  Run v -> fmap anyToUI <$> moveRunHuman True v
  Wait -> Right <$> fmap ReqUITimed waitHuman
  MoveItem cLegalRaw toCStore verbRaw _ auto ->
    fmap ReqUITimed <$> moveItemHuman cLegalRaw toCStore verbRaw auto
  Project ts -> fmap ReqUITimed <$> projectHuman ts
  Apply ts -> fmap ReqUITimed <$> applyHuman ts
  AlterDir ts -> fmap ReqUITimed <$> alterDirHuman ts
  TriggerTile ts -> fmap ReqUITimed <$> triggerTileHuman ts
  StepToTarget -> fmap anyToUI <$> stepToTargetHuman

  GameRestart t -> gameRestartHuman t
  GameExit -> gameExitHuman
  GameSave -> fmap Right gameSaveHuman
  Automate -> automateHuman

  -- Local.
  GameDifficultyCycle -> addNoSlides gameDifficultyCycle
  PickLeader k -> Left <$> pickLeaderHuman k
  MemberCycle -> Left <$> memberCycleHuman
  MemberBack -> Left <$> memberBackHuman
  DescribeItem cstore -> Left <$> describeItemHuman cstore
  AllOwned -> Left <$> allOwnedHuman
  SelectActor -> Left <$> selectActorHuman
  SelectNone -> addNoSlides selectNoneHuman
  Clear -> addNoSlides clearHuman
  Repeat n -> addNoSlides $ repeatHuman n
  Record -> Left <$> recordHuman
  History -> Left <$> historyHuman
  MarkVision -> addNoSlides markVisionHuman
  MarkSmell -> addNoSlides markSmellHuman
  MarkSuspect -> addNoSlides markSuspectHuman
  Help -> Left <$> helpHuman
  MainMenu -> Left <$> mainMenuHuman
  Macro _ kms -> addNoSlides $ macroHuman kms

  MoveCursor v k -> Left <$> moveCursorHuman v k
  TgtFloor -> Left <$> tgtFloorHuman
  TgtEnemy -> Left <$> tgtEnemyHuman
  TgtUnknown -> Left <$> tgtUnknownHuman
  TgtItem -> Left <$> tgtItemHuman
  TgtStair up -> Left <$> tgtStairHuman up
  TgtAscend k -> Left <$> tgtAscendHuman k
  EpsIncr b -> Left <$> epsIncrHuman b
  TgtClear -> Left <$> tgtClearHuman
  Cancel -> Left <$> cancelHuman mainMenuHuman
  Accept -> Left <$> acceptHuman helpHuman

addNoSlides :: Monad m => m () -> m (SlideOrCmd RequestUI)
addNoSlides cmdCli = cmdCli >> return (Left mempty)
