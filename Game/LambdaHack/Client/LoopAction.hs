{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopAI, loopUI) where

import Control.Monad
import qualified Data.Text as T

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Animation
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.State
import Game.LambdaHack.Utils.Assert

initCli :: MonadClient m => (State -> m ()) -> m (Either Msg Msg)
initCli putSt = do
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  restored <- restoreGame
  case restored of
    Left (s, cli, msg) -> do  -- Restore a game or at least history.
      let sCops = updateCOps (const cops) s
      putSt sCops
      putClient cli
      return $ Left msg
    Right msg -> do  -- First visit ever, use the initial state.
      return $ Right msg

loopAI :: (MonadClientConn CmdClientAI m)
       => (CmdClientAI -> m ()) -> m ()
loopAI cmdClientAISem = do
  side <- getsClient sside
  msg <- initCli $ \s -> cmdClientAISem $ CmdAtomicAI $ ResumeServerA s
  cmd1 <- readConnToClient
  case (msg, cmd1) of
    (Left _, CmdAtomicAI ResumeA{}) -> return ()
    (Left _, CmdAtomicAI RestartA{}) -> return ()  -- server savefile faulty
    (Right msg1, CmdAtomicAI ResumeA{}) ->
      error $ T.unpack $ "Savefile of client " <> showT side <> " not usable. Can't join the party. Please remove all savefiles manually and restart. Savefile subsystem said: " <> msg1
    (Right _, CmdAtomicAI RestartA{}) -> return ()
    _ -> assert `failure` (side, msg, cmd1)
  cmdClientAISem cmd1
  -- State and client state now valid.
  debugPrint $ "AI client" <+> showT side <+> "started."
  loop
  debugPrint $ "AI client" <+> showT side <+> "stopped."
 where
  loop = do
    cmd <- readConnToClient
    cmdClientAISem cmd
    quit <- getsClient squit
    when (not quit) loop

loopUI :: (MonadClientUI m, MonadClientConn CmdClientUI m)
       => (CmdClientUI -> m ()) -> m ()
loopUI cmdClientUISem = do
  side <- getsClient sside
  msg <- initCli $ \s -> cmdClientUISem $ CmdAtomicUI $ ResumeServerA s
  cmd1 <- readConnToClient
  case (msg, cmd1) of
    (Left msg1, CmdAtomicUI ResumeA{}) -> do
      cmdClientUISem cmd1
      savedFs <- getsClient sframe
      let firstFrame [] = Nothing
          firstFrame (AcConfirm _ : _) = Nothing  -- too many confirms
          firstFrame (AcRunning fr : _) = Just fr
          firstFrame (AcNormal fr : _) = Just fr
          firstFrame (AcDelay : fs) = firstFrame fs
          theSame _ AcConfirm{} = False  -- we need distinct confirmations
          theSame f1 (AcNormal fr) = f1 == fr
          theSame f1 (AcRunning fr) = f1 == fr
          theSame _ AcDelay = True
      case (firstFrame savedFs, firstFrame $ reverse savedFs) of
        (Just f1, Just _) | all (theSame f1) savedFs -> do
          -- These are usually just the AgeGameA frames.
          -- TODO: addMsg, no confirm, but don't add to history
          let fend = f1 {sfTop = msg1 <+> moreMsg}
              sframe = [AcConfirm fend] ++ savedFs
          modifyClient $ \cli -> cli {sframe}
        (Just f1, Just fk) -> do
          let fstart = fk {sfTop = "In the last episode:" <+> moreMsg}
              fend = f1 {sfTop = msg1 <+> moreMsg}
              sframe = [AcConfirm fend] ++ savedFs ++ [AcConfirm fstart]
          modifyClient $ \cli -> cli {sframe}
        _ -> return ()  -- possible only with multiple consecutive saves
    (Left _, CmdAtomicUI RestartA{}) -> do
      cmdClientUISem cmd1
      msgAdd $ "Server savefile is corrupted."
               <+> "Dropping the client savefile and starting a new game."
    (Right msg1, CmdAtomicUI ResumeA{}) ->
      error $ T.unpack $ "Savefile of client " <> showT side <> " not usable. Can't join the party. Please remove all savefiles manually and restart. Savefile subsystem said: " <> msg1
    (Right msg1, CmdAtomicUI RestartA{}) -> do
      cmdClientUISem cmd1
      msgAdd msg1
    _ -> assert `failure` (side, msg, cmd1)
  -- State and client state now valid.
  debugPrint $ "UI client" <+> showT side <+> "started."
  loop
  debugPrint $ "UI client" <+> showT side <+> "stopped."
  unlockUI
 where
  loop = do
    cmd <- readConnToClient
    cmdClientUISem cmd
    quit <- getsClient squit
    when (not quit) loop
