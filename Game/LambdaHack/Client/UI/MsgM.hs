-- | Monadic operations on game messages.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAddDuplicate, msgAdd, promptAddDuplicate, promptAdd1, promptAdd0
  , promptMainKeys, recordHistory
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Container
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State

-- | Add a message to the current report.
msgAddDuplicate :: MonadClientUI m => Text -> m Bool
msgAddDuplicate msg = do
  time <- getsState stime
  history <- getsSession shistory
  let (nhistory, duplicate) =
        addToReport history (toMsg $ textToAL msg) 1 time
  modifySession $ \sess -> sess {shistory = nhistory}
  return duplicate

-- | Add a message to the current report. Do not report if it was a duplicate.
msgAdd :: MonadClientUI m => Text -> m ()
msgAdd = void <$> msgAddDuplicate

-- | Add a prompt to the current report.
promptAddDuplicate :: MonadClientUI m => Text -> Int -> m Bool
promptAddDuplicate msg n = do
  time <- getsState stime
  history <- getsSession shistory
  let (nhistory, duplicate) =
        addToReport history (toPrompt $ textToAL msg) n time
  modifySession $ \sess -> sess {shistory = nhistory}
  return duplicate

-- | Add a prompt to the current report. Do not report if it was a duplicate.
promptAdd1 :: MonadClientUI m => Text -> m ()
promptAdd1 = void <$> flip promptAddDuplicate 1

-- | Add a prompt to the current report with 0 copies for the purpose
-- of collating cuplicates. Do not report if it was a duplicate.
promptAdd0 :: MonadClientUI m => Text -> m ()
promptAdd0 = void <$> flip promptAddDuplicate 0

-- | Add a prompt with basic keys description.
promptMainKeys :: MonadClientUI m => m ()
promptMainKeys = do
  side <- getsClient sside
  ours <- getsState $ fidActorNotProjGlobalAssocs side
  revCmd <- revCmdMap
  let kmHelp = revCmd (K.undefinedKM) HumanCmd.Hint
      kmViewEqp = revCmd (K.undefinedKM)
                         (HumanCmd.ChooseItemMenu (MStore CEqp))
      kmItemEqp = revCmd (K.undefinedKM)
                         (HumanCmd.MoveItem [CGround, CInv, CSha] CEqp
                                            Nothing False)
  saimMode <- getsSession saimMode
  UIOptions{uVi, uLaptop} <- getsSession sUIOptions
  xhair <- getsSession sxhair
  -- The silly "uk8o79jl" ordering of keys is chosen to match "hjklyubn",
  -- which the usual way of writing them.
  let moveKeys | uVi = "keypad or hjklyubn"
               | uLaptop = "keypad or uk8o79jl"
               | otherwise = "keypad"
      manyTeammates = length ours > 1
      keepTab = if manyTeammates
                then "Keep TAB of teammates (S-TAB for other levels)."
                else ""
      viewEquip = if eqpKeysAreNormal
                  then "View (E)quipment and (e)quip items."
                  else ""
      moreHelp = "Press" <+> tshow kmHelp <+> "for help."
      eqpKeysAreNormal = kmViewEqp == K.mkChar 'E'
                         && kmItemEqp == K.mkChar 'e'
      keys | isNothing saimMode =
        "Explore with" <+> moveKeys <+> "keys or mouse."
        <+> viewEquip
        <+> keepTab
        <+> moreHelp
           | otherwise =
        "Aim" <+> tgtKindDescription xhair
        <+> "with" <+> moveKeys <+> "keys or mouse."
        <+> keepTab
        <+> moreHelp
  void $ promptAdd0 keys

-- | Store new report in the history and archive old report.
recordHistory :: MonadClientUI m => m ()
recordHistory =
  modifySession $ \sess -> sess {shistory = archiveReport $ shistory sess}
