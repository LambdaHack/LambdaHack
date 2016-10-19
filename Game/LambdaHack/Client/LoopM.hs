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
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State

-- | The main game loop for an AI client.
loopAI :: ( MonadClientSetup m
          , MonadAtomic m
          , MonadClientReadResponse ResponseAI m
          , MonadClientWriteRequest RequestAI m )
       => DebugModeCli -> m ()
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
      handleResponseAI $ RespUpdAtomicAI $ UpdResumeServer sCops
      putClient cli {sdebugCli}
      return True
    _ -> return False
  cmd1 <- receiveResponse
  case (restored, cmd1) of
    (True, RespUpdAtomicAI UpdResume{}) -> return ()
    (True, RespUpdAtomicAI UpdRestart{}) -> return ()
    (False, RespUpdAtomicAI UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomicAI UpdRestart{}) -> return ()
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  handleResponseAI cmd1
  -- State and client state now valid.
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponseAI cmd
    quit <- getsClient squit
    unless quit loop

-- | The main game loop for a UI client.
loopUI :: ( MonadClientSetup m
          , MonadClientUI m
          , MonadAtomic m
          , MonadClientReadResponse ResponseUI m
          , MonadClientWriteRequest RequestUI m )
       => KeyKind -> Config -> DebugModeCli -> m ()
loopUI copsClient sconfig sdebugCli = do
  initUI copsClient sconfig sdebugCli
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  restoredG <- tryRestore False
  restored <- case restoredG of
    Just (s, cli, sess) | not $ snewGameCli sdebugCli -> do
      -- Restore game.
      let sCops = updateCOps (const cops) s
      handleResponseUI $ RespUpdAtomicUI $ UpdResumeServer sCops
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
    (True, RespUpdAtomicUI UpdResume{}) -> return ()
    (True, RespUpdAtomicUI UpdRestart{}) -> do
      msgAdd $
        "Ignoring an old savefile and starting a new game."
    (False, RespUpdAtomicUI UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomicUI UpdRestart{}) -> return ()
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  handleResponseUI cmd1
  -- State and client state now valid.
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponseUI cmd
    quit <- getsClient squit
    unless quit loop
