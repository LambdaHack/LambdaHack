-- | Monadic operations on game messages.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAddDuplicate, msgAdd, msgLnAdd, msgAdd0, promptAdd, promptAdd0
  , promptMainKeys, recordHistory
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Definition.Defs

-- | Add a message to the current report.
msgAddDuplicate :: MonadClientUI m => Text -> MsgClass -> Int -> m Bool
msgAddDuplicate msg msgClass n = do
  sUIOptions <- getsSession sUIOptions
  time <- getsState stime
  history <- getsSession shistory
  let mem = EM.fromList <$> uMessageColors sUIOptions
      (nhistory, duplicate) =
        addToReport history (toMsg mem msgClass msg) n time
  modifySession $ \sess -> sess {shistory = nhistory}
  return duplicate

-- | Add a message to the current report. Do not report if it was a duplicate.
msgAdd :: MonadClientUI m => MsgClass -> Text -> m ()
msgAdd msgClass msg = void $ msgAddDuplicate msg msgClass 1

-- | Add a message to the current report. End previously collected report,
-- if any, with newline.
msgLnAdd :: MonadClientUI m => MsgClass -> Text -> m ()
msgLnAdd msgClass msg = do
  history <- getsSession shistory
  msgAdd msgClass $ if nullFilteredReport $ newReport history
                    then msg
                    else T.cons '\n' msg

-- | Add a message to the current report with 0 copies for the purpose
-- of collating duplicates. Do not report if it was a duplicate.
msgAdd0 :: MonadClientUI m => MsgClass -> Text -> m ()
msgAdd0 msgClass msg = void $ msgAddDuplicate msg msgClass 0

-- | Add a prompt to the current report. Do not report if it was a duplicate.
promptAdd :: MonadClientUI m => Text -> m ()
promptAdd = msgAdd MsgAlert

-- | Add a prompt to the current report with 0 copies for the purpose
-- of collating duplicates. Do not report if it was a duplicate.
promptAdd0 :: MonadClientUI m => Text -> m ()
promptAdd0 = msgAdd0 MsgPrompt

-- | Add a prompt with basic keys description.
promptMainKeys :: MonadClientUI m => m ()
promptMainKeys = do
  side <- getsClient sside
  ours <- getsState $ fidActorNotProjGlobalAssocs side
  revCmd <- revCmdMap
  let kmHelp = revCmd K.undefinedKM HumanCmd.Hint
      kmViewStash = revCmd K.undefinedKM
                           (HumanCmd.ChooseItemMenu (MStore CStash))
      kmItemStash = revCmd K.undefinedKM
                           (HumanCmd.MoveItem [CGround, CEqp] CStash
                                              Nothing False)
      kmXhairPointerFloor = revCmd K.undefinedKM HumanCmd.XhairPointerFloor
  saimMode <- getsSession saimMode
  UIOptions{uVi, uLeftHand} <- getsSession sUIOptions
  xhair <- getsSession sxhair
  -- The silly "axwdqezc" name of keys is chosen to match "hjklyubn",
  -- which the usual way of writing them.
  let moveKeys | uVi && uLeftHand = "keypad or axwdqezc or hjklyubn"
               | uLeftHand = "keypad or axwdqezc"
               | uVi = "keypad or hjklyubn"
               | otherwise = "keypad"
      manyTeammates = length ours > 1
      keepTab = if manyTeammates
                then "Keep TAB of teammates (S-TAB for other levels)."
                else ""
      describePos = if mmbIsNormal
                    then "Describe map position with MMB."
                    else ""
      viewEquip = if stashKeysAreNormal
                  then "View shared (I)nventory stash and stash items into the (i)nventory."
                  else ""
      moreHelp = "Press" <+> tshow kmHelp <+> "for help."
      mmbIsNormal = kmXhairPointerFloor == K.middleButtonReleaseKM
      stashKeysAreNormal = kmViewStash == K.mkChar 'I'
                           && kmItemStash == K.mkChar 'i'
      keys | isNothing saimMode =
        "Explore with" <+> moveKeys <+> "or mouse."
        <+> describePos
        <+> viewEquip
        <+> keepTab
        <+> moreHelp
           | otherwise =
        tgtKindVerb xhair
        <+> "with" <+> moveKeys <+> "keys or mouse."
        <+> keepTab
        <+> moreHelp
  void $ promptAdd0 keys

tgtKindVerb :: Maybe Target -> Text
tgtKindVerb mtgt = case mtgt of
  Just TEnemy{} -> "Aim at enemy"
  Just TNonEnemy{} -> "Aim at non-enemy"
  Just TPoint{} -> "Aim at position"
  Just TVector{} -> "Indicate a move vector"
  Nothing -> "Start aiming"

-- | Store new report in the history and archive old report.
recordHistory :: MonadClientUI m => m ()
recordHistory =
  modifySession $ \sess -> sess {shistory = archiveReport $ shistory sess}
