-- | Monadic operations on game messages.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAddDuplicate, msgAdd, promptAddDuplicate, promptAdd
  , promptMainKeys, recordHistory
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State

-- | Add a message to the current report.
msgAddDuplicate :: MonadClientUI m => Text -> m Bool
msgAddDuplicate msg = do
  history <- getsSession shistory
  let (nhistory, duplicate) = addToReport history (toMsg $ textToAL msg)
  modifySession $ \sess -> sess {shistory = nhistory}
  return duplicate

-- | Add a message to the current report. Do not report if it was a duplicate.
msgAdd :: MonadClientUI m => Text -> m ()
msgAdd = void <$> msgAddDuplicate

-- | Add a prompt to the current report.
promptAddDuplicate :: MonadClientUI m => Text -> m Bool
promptAddDuplicate msg = do
  history <- getsSession shistory
  let (nhistory, duplicate) = addToReport history (toPrompt $ textToAL msg)
  modifySession $ \sess -> sess {shistory = nhistory}
  return duplicate

-- | Add a prompt to the current report. Do not report if it was a duplicate.
promptAdd :: MonadClientUI m => Text -> m ()
promptAdd = void <$> promptAddDuplicate

-- | Add a prompt with basic keys description.
promptMainKeys :: MonadClientUI m => m ()
promptMainKeys = do
  revCmd <- revCmdMap
  let km = revCmd (K.mkChar '?') HumanCmd.Hint
  saimMode <- getsSession saimMode
  UIOptions{uVi, uLaptop} <- getsSession sUIOptions
  xhair <- getsSession sxhair
  -- The silly "uk8o79jl" ordering of keys is chosen to match "hjklyubn",
  -- which the usual way of writing them.
  let moveKeys | uVi = "keypad or hjklyubn"
               | uLaptop = "keypad or uk8o79jl"
               | otherwise = "keypad"
      moreHelp = "Press" <+> tshow km <+> "for help."
      keys | isNothing saimMode =
        "Explore with" <+> moveKeys <+> "keys or mouse." <+> moreHelp
           | otherwise =
        "Aim" <+> tgtKindDescription xhair
        <+> "with" <+> moveKeys <+> "keys or mouse." <+> moreHelp
  void $ promptAdd keys

-- | Store new report in the history and archive old report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  time <- getsState stime
  history <- getsSession shistory
  modifySession $ \sess -> sess {shistory = archiveReport history time}
