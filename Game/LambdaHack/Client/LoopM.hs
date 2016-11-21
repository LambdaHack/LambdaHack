{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( loopAI, loopUI
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

-- | The main game loop for an AI client.
loopAI :: ( MonadClientSetup m
          , MonadClientUI m
          , MonadAtomic m
          , MonadClientReadResponse m
          , MonadClientWriteRequest m )
       => DebugModeCli -> m ()
{-# INLINE loopAI #-}
loopAI sdebugCli = do
  initAI sdebugCli
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  side <- getsClient sside
  cops <- getsState scops
  restoredG <- tryRestore True
  restored <- case restoredG of
    Just (s, cli, ()) | not $ snewGameCli sdebugCli -> do  -- Restore game.
      let sCops = updateCOps (const cops) s
      handleResponseAI $ RespUpdAtomic False $ UpdResumeServer sCops
      putClient cli {sdebugCli}
      return True
    _ -> return False
  cmd1 <- receiveResponse
  case (restored, cmd1) of
    (True, RespUpdAtomic _ UpdResume{}) -> return ()
    (True, RespUpdAtomic _ UpdRestart{}) -> return ()
    (False, RespUpdAtomic _ UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomic _ UpdRestart{}) -> return ()
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  handleResponseAI cmd1
  -- State and client state now valid.
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "stopped."
 where
  loop = {-# SCC loopAI #-} do
    cmd <- receiveResponse
    handleResponseAI cmd
    quit <- getsClient squit
    unless quit loop

-- | The main game loop for a UI client.
loopUI :: ( MonadClientSetup m
          , MonadClientUI m
          , MonadAtomic m
          , MonadClientReadResponse m
          , MonadClientWriteRequest m )
       => KeyKind -> Config -> DebugModeCli -> Bool -> m ()
{-# INLINE loopUI #-}
loopUI copsClient sconfig sdebugCli isAI = do
  if isAI then initAI sdebugCli else initUI copsClient sconfig sdebugCli
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  restoredG <- tryRestore False
  restored <- case restoredG of
    Just (s, cli, sess) | not $ snewGameCli sdebugCli -> do
      -- Restore game.
      let sCops = updateCOps (const cops) s
      handleResponseUI $ RespUpdAtomic True $ UpdResumeServer sCops
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
    (True, RespUpdAtomic _ UpdResume{}) -> return ()
    (True, RespUpdAtomic _ UpdRestart{}) -> do
      msgAdd $
        "Ignoring an old savefile and starting a new game."
    (False, RespUpdAtomic _ UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomic _ UpdRestart{}) -> return ()
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  handleResponseUI cmd1
  -- State and client state now valid.
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "stopped."
 where
  loop = {-# SCC loopUI #-} do
    cmd <- receiveResponse
    handleResponseUI cmd
--    quit <- getsClient squit
    loop
