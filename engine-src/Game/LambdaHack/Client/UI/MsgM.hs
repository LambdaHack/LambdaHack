-- | Monadic operations on game messages.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAddDuplicate, msgAddDistinct, msgAdd, msgLnAdd
  , promptMainKeys, recordHistory
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.EffectDescription
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

-- | Add a shared message to the current report. Say if it was a duplicate.
msgAddDuplicate :: (MonadClientUI m, MsgShared a) => a -> Text -> m Bool
msgAddDuplicate msgClass t = do
  sUIOptions <- getsSession sUIOptions
  time <- getsState stime
  history <- getsSession shistory
  curTutorial <- getsSession scurTutorial
  overrideTut <- getsSession soverrideTut
  let displayTutorialHints = fromMaybe curTutorial overrideTut
      msg = toMsgShared (uMessageColors sUIOptions) msgClass t
      (nhistory, duplicate) = addToReport displayTutorialHints history msg time
  modifySession $ \sess -> sess {shistory = nhistory}
  return duplicate

-- | Add a message comprising of two different texts, one to show, the other
-- to save to messages log, to the current report.
msgAddDistinct :: (MonadClientUI m) => MsgClassDistinct -> (Text, Text) -> m ()
msgAddDistinct msgClass (t1, t2) = do
  sUIOptions <- getsSession sUIOptions
  time <- getsState stime
  history <- getsSession shistory
  curTutorial <- getsSession scurTutorial
  overrideTut <- getsSession soverrideTut
  let displayTutorialHints = fromMaybe curTutorial overrideTut
      msg = toMsgDistinct (uMessageColors sUIOptions) msgClass t1 t2
      (nhistory, _) = addToReport displayTutorialHints history msg time
  modifySession $ \sess -> sess {shistory = nhistory}

-- | Add a message to the current report.
msgAdd :: (MonadClientUI m, MsgShared a) => a -> Text -> m ()
msgAdd msgClass t = void $ msgAddDuplicate msgClass t

-- | Add a message to the current report. End previously collected report,
-- if any, with newline.
msgLnAdd :: (MonadClientUI m, MsgShared a) => a -> Text -> m ()
msgLnAdd msgClass t = do
  modifySession $ \sess -> sess {shistory = addEolToNewReport $ shistory sess}
  msgAdd msgClass t

-- | Add a prompt with basic keys description.
promptMainKeys :: MonadClientUI m => m ()
promptMainKeys = do
  side <- getsClient sside
  ours <- getsState $ fidActorNotProjGlobalAssocs side
  revCmd <- revCmdMap
  let kmHelp = revCmd HumanCmd.Hint
      kmViewStash = revCmd (HumanCmd.ChooseItemMenu (MStore CStash))
      kmItemStash = revCmd (HumanCmd.MoveItem [CGround, CEqp] CStash
                                              Nothing False)
      kmXhairPointerFloor = revCmd HumanCmd.XhairPointerFloor
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
      detailAtDefault = (detailLevel <$> saimMode) == Just defaultDetailLevel
      -- @Tab@ here is not a button, which we would write consistently
      -- as @TAB@, just as in our internal in-game key naming, but a key name
      -- as written on the keyboard, hence most useful to a newbie.
      keepTab = if manyTeammates
                then "Switch to another teammate with Tab, while all others auto-melee foes, if adjacent, but normally don't chase them."
                else ""
      describePos = if describeIsNormal
                    then "Describe map position with MMB or RMB."
                    else ""
      viewEquip = if stashKeysAreNormal
                  then "View shared 'I'nventory stash and stash items into the 'i'nventory."
                  else ""
      moreHelp = "Press '" <> tshow kmHelp <> "' for more help."
      describeIsNormal = kmXhairPointerFloor == K.middleButtonReleaseKM
      stashKeysAreNormal = kmViewStash == K.mkChar 'I'
                           && kmItemStash == K.mkChar 'i'
      keys | isNothing saimMode =
        "Explore with" <+> moveKeys <+> "or mouse."
        <+> describePos
        <+> viewEquip
        <+> keepTab
        <+> moreHelp
           | otherwise =
        (if detailAtDefault then "" else miniHintAiming)
        <+> tgtKindVerb xhair
        <+> "with" <+> moveKeys <+> "keys or mouse."
        <+> keepTab
        <+> moreHelp
  void $ msgAdd MsgPromptGeneric keys

tgtKindVerb :: Maybe Target -> Text
tgtKindVerb mtgt = case mtgt of
  Just TEnemy{} -> "Aim at enemy"
  Just TNonEnemy{} -> "Aim at non-enemy"
  Just TPoint{} -> "Aim at position"
  Just TVector{} -> "Indicate a move vector"
  Nothing -> "Start aiming"

-- | Store new report in the history and archive old report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  UIOptions{uHistory1PerLine} <- getsSession sUIOptions
  modifySession $ \sess ->
    sess {shistory = archiveReport uHistory1PerLine $ shistory sess}
