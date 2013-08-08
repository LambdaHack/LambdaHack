{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopAI, loopUI) where

import Control.Monad
import qualified Data.Text as T

import Game.LambdaHack.Client.Action
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

loopAI :: (MonadConnClient CmdClientAI m)
       => (CmdClientAI -> m ()) -> m ()
loopAI cmdClientAISem = do
  side <- getsClient sside
  ConnServer{readConnServer} <- getConn
  msg <- initCli $ \s -> cmdClientAISem $ CmdAtomicAI $ ResumeServerA s
  cmd1 <- readConnServer
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
    ConnServer{readConnServer} <- getConn
    cmd <- readConnServer
    cmdClientAISem cmd
    quit <- getsClient squit
    when (not quit) loop

loopUI :: (MonadClientUI m, MonadConnClient CmdClientUI m)
       => (CmdClientUI -> m ()) -> m ()
loopUI cmdClientUISem = do
  side <- getsClient sside
  ConnServer{readConnServer} <- getConn
  msg <- initCli $ \s -> cmdClientUISem $ CmdAtomicUI $ ResumeServerA s
  cmd1 <- readConnServer
  case (msg, cmd1) of
    (Left _msg1, CmdAtomicUI ResumeA{}) -> do
      cmdClientUISem cmd1
      -- TODO: use msg1
    (Left _, CmdAtomicUI RestartA{}) -> do
      cmdClientUISem cmd1
      msgAdd $ "Starting a new game (and ignoring an old client savefile)."
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
 where
  loop = do
    ConnServer{readConnServer} <- getConn
    cmd <- readConnServer
    cmdClientUISem cmd
    quit <- getsClient squit
    when (not quit) loop
