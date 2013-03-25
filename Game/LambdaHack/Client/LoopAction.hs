{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopAI, loopUI) where

import Control.Monad
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.State
import Game.LambdaHack.ClientCmd
import Game.LambdaHack.Msg
import Game.LambdaHack.State
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
      -- TODO: create or restore from config clients RNG seed
      return $ Right msg

loopAI :: (MonadClient m, MonadClientConn CmdClientAI m)
       => (CmdClientAI -> m ()) -> m ()
loopAI cmdClientAISem = do
  side <- getsClient sside
  msg <- initCli $ \s -> cmdClientAISem $ CmdAtomicAI $ ResumeServerA s
  cmd1 <- readConnToClient
  case (msg, cmd1) of
    (Left _, CmdAtomicAI ResumeA{}) -> return ()
    (Left _, CmdAtomicAI RestartA{}) -> return ()  -- server savegame faulty
    (Right msg1, CmdAtomicAI ResumeA{}) ->
      error $ T.unpack $ "Savefile of client " <> showT side <> " not usable. Can't join the party. Please remove all savefiles manually and restart. Savefile subsystem said: " <> msg1
    (Right _, CmdAtomicAI RestartA{}) -> return ()
    _ -> assert `failure` (side, msg, cmd1)
  cmdClientAISem cmd1
  -- State and client state now valid.
  loop
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
    (Left _, CmdAtomicUI ResumeA{}) -> return ()
    (Left _, CmdAtomicUI RestartA{}) -> return ()  -- server savegame faulty
    (Right msg1, CmdAtomicUI ResumeA{}) ->
      error $ T.unpack $ "Savefile of client " <> showT side <> " not usable. Can't join the party. Please remove all savefiles manually and restart. Savefile subsystem said: " <> msg1
    (Right _, CmdAtomicUI RestartA{}) -> return ()
    _ -> assert `failure` (side, msg, cmd1)
  cmdClientUISem cmd1
  either msgAdd msgAdd msg
  -- State and client state now valid.
  loop
 where
  loop = do
    cmd <- readConnToClient
    cmdClientUISem cmd
    quit <- getsClient squit
    when (not quit) loop
