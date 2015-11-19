-- | Semantics of human player commands.
module Game.LambdaHack.Client.UI.HandleHumanClient
  ( cmdHumanSem
  ) where

import Prelude ()
import Prelude.Compat

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
cmdHumanSem cmd =
  if noRemoteHumanCmd cmd then do
    -- If in targeting mode, check if the current level is the same
    -- as player level and refuse performing the action otherwise.
    arena <- getArenaUI
    lidV <- viewedLevel
    if arena /= lidV then
      failWith "command disabled on a remote level, press ESC to switch back"
    else cmdAction cmd
  else cmdAction cmd

-- | Compute the basic action for a command and mark whether it takes time.
cmdAction :: MonadClientUI m => HumanCmd -> m (SlideOrCmd RequestUI)
cmdAction cmd = case cmd of
  -- Global.
  Move v -> fmap anyToUI <$> moveRunHuman True True False False v
  Run v -> fmap anyToUI <$> moveRunHuman True True True True v
  Wait -> Right <$> fmap ReqUITimed waitHuman
  MoveItem cLegalRaw toCStore mverb _ auto ->
    fmap ReqUITimed <$> moveItemHuman cLegalRaw toCStore mverb auto
  DescribeItem cstore -> fmap ReqUITimed <$> describeItemHuman cstore
  Project ts -> fmap ReqUITimed <$> projectHuman ts
  Apply ts -> fmap ReqUITimed <$> applyHuman ts
  AlterDir ts -> fmap ReqUITimed <$> alterDirHuman ts
  TriggerTile ts -> fmap ReqUITimed <$> triggerTileHuman ts
  RunOnceAhead -> fmap anyToUI <$> runOnceAheadHuman
  MoveOnceToCursor -> fmap anyToUI <$> moveOnceToCursorHuman
  RunOnceToCursor  -> fmap anyToUI <$> runOnceToCursorHuman
  ContinueToCursor -> fmap anyToUI <$> continueToCursorHuman

  GameRestart t -> gameRestartHuman t
  GameExit -> gameExitHuman
  GameSave -> fmap Right gameSaveHuman
  Tactic -> tacticHuman
  Automate -> automateHuman
  MainMenu -> mainMenuHuman cmdAction
  Help -> helpHuman cmdAction
  GameDifficultyIncr k -> gameDifficultyIncr k >> mainMenuHuman cmdAction
  Cancel -> cancelHuman $ mainMenuHuman cmdAction
  Accept -> acceptHuman $ helpHuman cmdAction

  -- Local.
  PickLeader k -> Left <$> pickLeaderHuman k
  MemberCycle -> Left <$> memberCycleHuman
  MemberBack -> Left <$> memberBackHuman
  SelectActor -> addNoSlides selectActorHuman
  SelectNone -> addNoSlides selectNoneHuman
  Clear -> addNoSlides clearHuman
  StopIfTgtMode -> addNoSlides stopIfTgtModeHuman
  SelectWithPointer -> addNoSlides selectWithPointer
  Repeat n -> addNoSlides $ repeatHuman n
  Record -> Left <$> recordHuman
  History -> Left <$> historyHuman
  MarkVision -> addNoSlides markVisionHuman
  MarkSmell -> addNoSlides markSmellHuman
  MarkSuspect -> addNoSlides markSuspectHuman
  Macro _ kms -> Left <$> macroHuman kms
  Alias _ cmd2 -> cmdAction cmd2

  MoveCursor v k -> Left <$> moveCursorHuman v k
  TgtFloor -> Left <$> tgtFloorHuman
  TgtEnemy -> Left <$> tgtEnemyHuman
  TgtAscend k -> Left <$> tgtAscendHuman k
  EpsIncr b -> Left <$> epsIncrHuman b
  TgtClear -> Left <$> tgtClearHuman
  CursorUnknown -> Left <$> cursorUnknownHuman
  CursorItem -> Left <$> cursorItemHuman
  CursorStair up -> Left <$> cursorStairHuman up
  CursorPointerFloor -> addNoSlides cursorPointerFloorHuman
  CursorPointerEnemy -> addNoSlides cursorPointerEnemyHuman
  TgtPointerFloor -> Left <$> tgtPointerFloorHuman
  TgtPointerEnemy -> Left <$> tgtPointerEnemyHuman

addNoSlides :: Monad m => m () -> m (SlideOrCmd RequestUI)
addNoSlides cmdCli = cmdCli >> return (Left mempty)
