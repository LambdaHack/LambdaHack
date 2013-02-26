{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopCli, loopUI) where

import Control.Monad

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

initCli :: (MonadAction m, MonadClientChan m)
        => (CmdCli -> m ()) -> (CmdUI -> m ()) -> m Msg
initCli cmdCliSem cmdUISem = do
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  writeChanToSer []  -- tell the server the client is ready
  cops <- getsState scops
  restored <- restoreGame
  case restored of
    Right msg -> do  -- First visit ever, use the initial state.
      -- TODO: create or restore from config clients RNG seed
      let expected cmd = case cmd of RestartA{} -> True; _ -> False
      expectCmd cmdCliSem cmdUISem expected
      return msg
    Left (s, cli, msg) -> do  -- Restore a game or at least history.
      let sCops = updateCOps (const cops) s
      putState sCops
      putClient cli
      let expected cmd = case cmd of ResumeA{} -> True; _ -> False
      expectCmd cmdCliSem cmdUISem expected
      modifyClient $ \cli2 -> cli2 {squit = False}
      return msg
  -- State and client state are now valid.

expectCmd :: MonadClientChan m
           => (CmdCli -> m ()) -> (CmdUI -> m ()) -> (CmdAtomic -> Bool)
           -> m ()
expectCmd cmdCliSem cmdUISem expected = do
  side <- getsClient sside
  cmd1 <- readChanFromSer
  case cmd1 of
    Left (CmdAtomicCli cmd) | expected cmd -> cmdCliSem $ CmdAtomicCli cmd
    Right (CmdAtomicUI cmd) | expected cmd -> cmdUISem $ CmdAtomicUI cmd
    _ -> assert `failure` (side, cmd1)

loopCli :: (MonadAction m, MonadClientChan m)
        => (CmdCli -> m ()) -> m ()
loopCli cmdCliSem = do
  void $ initCli cmdCliSem undefined
  loop
 where
  loop = do
    side <- getsClient sside
    cmd2 <- readChanFromSer
    case cmd2 of
      Left cmd -> cmdCliSem cmd
      Right _ -> assert `failure` (side, cmd2)
    quit <- getsClient squit
    when (not quit) loop

loopUI :: (MonadAction m, MonadClientUI m, MonadClientChan m)
       => (CmdCli -> m ()) -> (CmdUI -> m ()) -> m ()
loopUI cmdCliSem cmdUISem = do
  msg <- initCli cmdCliSem cmdUISem
  msgAdd msg
  loop
 where
  loop = do
    cmd4 <- readChanFromSer
    case cmd4 of
      Left cmd -> cmdCliSem cmd
      Right cmd -> cmdUISem cmd
    quit <- getsClient squit
    when (not quit) loop
