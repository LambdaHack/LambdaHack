-- | Monadic operations on game messages.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAddDuplicate, msgAdd, promptAddDuplicate, promptAdd1, promptAdd0
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
  void $ promptAdd0 keys

-- | Store new report in the history and archive old report.
recordHistory :: MonadClientUI m => m ()
recordHistory =
  modifySession $ \sess -> sess {shistory = archiveReport $ shistory sess}
