-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MsgM
  ( splitOKX, msgAdd, promptAdd, promptAddAttr, recordHistory
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State

splitOKX :: MonadClientUI m => Y -> OKX -> m Slideshow
splitOKX y okx = do
  lid <- getArenaUI
  Level{lxsize} <- getLevel lid  -- TODO: screen length or viewLevel
  report <- getReportUI
  return $! splitOverlay lxsize y report okx

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Text -> m ()
msgAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toMsg $ toAttrLine msg)}

-- | Add a prompt to the current report.
promptAdd :: MonadClientUI m => Text -> m ()
promptAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt $ toAttrLine msg)}

-- | Add a prompt to the current report.
promptAddAttr :: MonadClientUI m => AttrLine -> m ()
promptAddAttr msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt msg)}

-- | Store current report in the history and reset report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  time <- getsState stime
  SessionUI{_sreport, shistory} <- getSession
  unless (nullReport _sreport) $ do
    let nhistory = addReport shistory time _sreport
    modifySession $ \sess -> sess { _sreport = emptyReport
                                  , shistory = nhistory }
