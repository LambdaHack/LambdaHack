{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
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

initCli :: (MonadAction m, MonadClient m, MonadClientChan c m)
        => m (Either Msg Msg)
initCli = do
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  writeChanToSer []  -- tell the server the client is ready
  cops <- getsState scops
  restored <- restoreGame
  case restored of
    Left (s, cli, msg) -> do  -- Restore a game or at least history.
      let sCops = updateCOps (const cops) s
      putState sCops
      putClient cli
      return $ Left msg
    Right msg -> do  -- First visit ever, use the initial state.
      -- TODO: create or restore from config clients RNG seed
      return $ Right msg

loopCli :: (MonadAction m, MonadClient m, MonadClientChan CmdCli m)
        => (CmdCli -> m ()) -> m ()
loopCli cmdCliSem = do
  side <- getsClient sside
  msg <- initCli
  cmd1 <- readChanFromSer
  case (msg, cmd1) of
    (Left _ , CmdAtomicCli ResumeA{}) -> cmdCliSem cmd1
    (Right _, CmdAtomicCli RestartA{}) -> cmdCliSem cmd1
    _ -> assert `failure` (side, msg, cmd1)
  -- State and client state now valid.
  loop
 where
  loop = do
    cmd <- readChanFromSer
    cmdCliSem cmd
    quit <- getsClient squit
    when (not quit) loop

loopUI :: (MonadAction m, MonadClientUI m, MonadClientChan CmdUI m)
       => (CmdUI -> m ()) -> m ()
loopUI cmdUISem = do
  side <- getsClient sside
  msg <- initCli
  cmd1 <- readChanFromSer
  case (msg, cmd1) of
    (Left msg1, CmdAtomicUI ResumeA{}) -> cmdUISem cmd1 >> msgAdd msg1
    (Right msg1, CmdAtomicUI RestartA{}) -> cmdUISem cmd1 >> msgAdd msg1
    _ -> assert `failure` (side, msg, cmd1)
  -- State and client state now valid.
  loop
 where
  loop = do
    cmd <- readChanFromSer
    cmdUISem cmd
    quit <- getsClient squit
    when (not quit) loop
