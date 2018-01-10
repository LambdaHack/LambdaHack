-- | Monadic operations on game messages.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAdd, promptAdd, promptMainKeys, promptAddAttr, recordHistory
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Map.Strict as M

import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.KeyBindings
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Text -> m ()
msgAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (sreport sess) (toMsg $ textToAL msg)}

-- | Add a prompt to the current report.
promptAdd :: MonadClientUI m => Text -> m ()
promptAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (sreport sess) (toPrompt $ textToAL msg)}

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
  promptAdd keys

-- | Add a prompt to the current report.
promptAddAttr :: MonadClientUI m => AttrLine -> m ()
promptAddAttr msg = modifySession $ \sess ->
  sess {_sreport = snocReport (sreport sess) (toPrompt msg)}

-- | Store current report in the history and reset report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  time <- getsState stime
  sessionUI <- getSession
  unless (nullReport $ sreport sessionUI) $ do
    let nhistory = addReport (shistory sessionUI) time (sreport sessionUI)
    modifySession $ \sess -> sess { _sreport = emptyReport
                                  , shistory = nhistory }
