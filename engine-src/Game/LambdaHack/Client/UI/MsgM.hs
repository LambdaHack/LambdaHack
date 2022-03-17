-- | Monadic operations on game messages.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAddDuplicate, msgAddDistinct, msgAdd, msgLnAdd
  , promptMainKeys, recordHistory, tutorialHintMsgAdd
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.TutorialHints
  (TutorialHints, renderTutorialHints)
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Definition.Defs

sniffMessages :: Bool
sniffMessages = False

-- | Add a shared message to the current report. Say if it was a duplicate.
msgAddDuplicate :: (MonadClientUI m, MsgShared a) => a -> Text -> m Bool
msgAddDuplicate msgClass t = do
  sUIOptions <- getsSession sUIOptions
  time <- getsState stime
  history <- getsSession shistory
  curTutorial <- getsSession scurTutorial
  overrideTut <- getsSession soverrideTut
  usedHints <- getsSession susedHints
  lid <- getArenaUI
  condInMelee <- condInMeleeM lid
  smuteMessages <- getsSession smuteMessages
  let displayHints = fromMaybe curTutorial overrideTut
      msg = toMsgShared (uMessageColors sUIOptions) msgClass t
      (nusedHints, nhistory, duplicate) =
        addToReport usedHints displayHints condInMelee history msg time
  unless smuteMessages $ do
    modifySession $ \sess -> sess {shistory = nhistory, susedHints = nusedHints}
    when sniffMessages $ clientPrintUI t
  return duplicate

-- | Add a message comprising of two different texts, one to show, the other
-- to save to messages log, to the current report.
msgAddDistinct :: MonadClientUI m => MsgClassDistinct -> (Text, Text) -> m ()
msgAddDistinct msgClass (t1, t2) = do
  sUIOptions <- getsSession sUIOptions
  time <- getsState stime
  history <- getsSession shistory
  curTutorial <- getsSession scurTutorial
  overrideTut <- getsSession soverrideTut
  usedHints <- getsSession susedHints
  lid <- getArenaUI
  condInMelee <- condInMeleeM lid
  smuteMessages <- getsSession smuteMessages
  let displayHints = fromMaybe curTutorial overrideTut
      msg = toMsgDistinct (uMessageColors sUIOptions) msgClass t1 t2
      (nusedHints, nhistory, _) =
        addToReport usedHints displayHints condInMelee history msg time
  unless smuteMessages $ do
    modifySession $ \sess -> sess {shistory = nhistory, susedHints = nusedHints}
    when sniffMessages $ clientPrintUI t1

-- | Add a message to the current report.
msgAdd :: (MonadClientUI m, MsgShared a) => a -> Text -> m ()
msgAdd msgClass t = void $ msgAddDuplicate msgClass t

-- | Add a tutorialHint message to the current report.
tutorialHintMsgAdd :: (MonadClientUI m) => TutorialHints -> m ()
tutorialHintMsgAdd tutorialHint = msgAdd MsgTutorialHint  (renderTutorialHints tutorialHint)

-- | Add a message to the current report. End previously collected report,
-- if any, with newline.
msgLnAdd :: (MonadClientUI m, MsgShared a) => a -> Text -> m ()
msgLnAdd msgClass t = do
  smuteMessages <- getsSession smuteMessages
  unless smuteMessages $
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
  miniHintAiming <- getMiniHintAiming
  -- The silly "axwdqezc" name of keys is chosen to match "hjklyubn",
  -- which the usual way of writing them.
  let moveKeys | uVi && uLeftHand = "keypad or axwdqezc or hjklyubn"
               | uLeftHand = "keypad or axwdqezc"
               | uVi = "keypad or hjklyubn"
               | otherwise = "keypad"
      manyTeammates = length ours > 1
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
        miniHintAiming
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
recordHistory =
  modifySession $ \sess -> sess {shistory = archiveReport $ shistory sess}
