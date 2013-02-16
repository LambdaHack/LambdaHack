{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopCli, loopUI) where

import Control.Monad
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

initCli :: (MonadAction m, MonadClientChan m) => (CmdCli -> m ()) -> m ()
initCli cmdCliSem = do
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  restored <- restoreGame
  case restored of
    Right msg -> do  -- First visit ever, use the initial state.
      -- TODO: create or restore from config clients RNG seed
      msgAdd msg
      let expected cmd = case cmd of RestartCli{} -> True; _ -> False
      expectCmd cmdCliSem expected
    Left (s, cli, msg) -> do  -- Restore a game or at least history.
      let sCops = updateCOps (const cops) s
      putState sCops
      putClient cli
      msgAdd msg
      let expected cmd = case cmd of ContinueSavedCli{} -> True; _ -> False
      expectCmd cmdCliSem expected
  modifyClient $ \cli -> cli {squit = Nothing}
  -- State and client state are now valid.

expectCmd :: MonadClientChan m
           => (CmdCli -> m ()) -> (CmdUpdateCli -> Bool) -> m ()
expectCmd cmdCliSem expected = do
  side <- getsClient sside
  cmd1 <- readChanFromSer
  case cmd1 of
    Left (CmdUpdateCli cmd) | expected cmd -> cmdCliSem (CmdUpdateCli cmd)
    _ -> assert `failure` (side, cmd1)

loopCli :: (MonadAction m, MonadClientChan m)
        => (CmdCli -> m ()) -> m ()
loopCli cmdCliSem = do
  initCli cmdCliSem
  loop
 where
  loop = do
    side <- getsClient sside
    cmd2 <- readChanFromSer
    case cmd2 of
      Left cmd -> cmdCliSem cmd
      Right _ -> assert `failure` (side, cmd2)
    quit <- getsClient squit
    when (isNothing quit) loop

loopUI :: (MonadAction m, MonadClientUI m, MonadClientChan m)
       => (CmdCli -> m ()) -> (CmdUI -> m ()) -> m ()
loopUI cmdCliSem cmdUISem = do
  initCli cmdCliSem
  loop
 where
  loop = do
    cmd4 <- readChanFromSer
    case cmd4 of
      Left cmd -> cmdCliSem cmd
      Right cmd -> cmdUISem cmd
    quit <- getsClient squit
    when (isNothing quit) loop
