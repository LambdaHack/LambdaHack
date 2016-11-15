-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MsgM
  ( msgAdd, promptAdd, promptAddAttr, recordHistory
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Text -> m ()
{-# INLINABLE msgAdd #-}
msgAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toMsg $ textToAL msg)}

-- | Add a prompt to the current report.
promptAdd :: MonadClientUI m => Text -> m ()
{-# INLINABLE promptAdd #-}
promptAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt $ textToAL msg)}

-- | Add a prompt to the current report.
promptAddAttr :: MonadClientUI m => AttrLine -> m ()
{-# INLINABLE promptAddAttr #-}
promptAddAttr msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt msg)}

-- | Store current report in the history and reset report.
recordHistory :: MonadClientUI m => m ()
{-# INLINABLE recordHistory #-}
recordHistory = do
  time <- getsState stime
  SessionUI{_sreport, shistory} <- getSession
  unless (nullReport _sreport) $ do
    let nhistory = addReport shistory time _sreport
    modifySession $ \sess -> sess { _sreport = emptyReport
                                  , shistory = nhistory }
