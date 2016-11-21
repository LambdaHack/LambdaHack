{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( loopUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.HandleResponseM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolM
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State

-- | The main game loop for an AI or UI client.
loopUI :: ( MonadClientSetup m
          , MonadClientUI m
          , MonadAtomic m
          , MonadClientReadResponse m
          , MonadClientWriteRequest m )
       => KeyKind -> Config -> DebugModeCli -> Bool -> m ()
{-# INLINABLE loopUI #-}
loopUI copsClient sconfig sdebugCli isAI = do
  if isAI then initAI sdebugCli else initUI copsClient sconfig sdebugCli
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  restoredG <- tryRestore
  restored <- case restoredG of
    Just (s, cli, sess) | not $ snewGameCli sdebugCli -> do
      -- Restore game.
      let sCops = updateCOps (const cops) s
      handleResponse $ RespUpdAtomic $ UpdResumeServer sCops
      schanF <- getsSession schanF
      sbinding <- getsSession sbinding
      putSession sess {schanF, sbinding}
      putClient cli {sdebugCli}
      return True
    Just (_, _, sessR) -> do
      -- Preserve the previous history, if any (--newGame).
      modifySession $ \sess -> sess {shistory = shistory sessR}
      return False
    _ -> return False
  side <- getsClient sside
  cmd1 <- receiveResponse
  case (restored, cmd1) of
    (True, RespUpdAtomic UpdResume{}) -> return ()
    (True, RespUpdAtomic UpdRestart{}) ->
      unless isAI $ msgAdd $
        "Ignoring an old savefile and starting a new game."
    (False, RespUpdAtomic UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomic UpdRestart{}) -> return ()
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  handleResponse cmd1
  -- State and client state now valid.
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponse cmd
    quit <- getsClient squit
    unless quit loop
