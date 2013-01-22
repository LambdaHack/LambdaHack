{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopCli2, loopCli4) where

import Data.Dynamic

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

initCli :: MonadClient m => m ()
initCli = do
  side <- getsState sside
  cops@Kind.COps{corule} <- getsState scops
  configUI <- getsClient sconfigUI
  let pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
      title = rtitle $ Kind.stdRuleset corule
  restored <- restoreGame (showT side) configUI pathsDataFile title
  case restored of
    Right msg -> do  -- First visit ever, use the initial state.
      -- TODO: create or restore from config clients RNG seed
      msgAdd msg
      -- TODO: somehow check that RestartCli arrives before any other cmd
    Left (s, cli, msg) -> do  -- Restore a game or at least history.
      let sCops = updateCOps (const cops) s
      putState sCops
      putClient cli
      msgAdd msg
      -- TODO: somehow check that ContinueSave arrives before any other cmd

loopCli2 :: MonadClientChan m
         => (CmdUpdateCli -> m ())
         -> (forall a. Typeable a => CmdQueryCli a -> m a)
         -> m ()
loopCli2 cmdUpdateCli cmdQueryCli = do
  initCli
  loop
 where
  loop = do
    side <- getsState sside
    cmd2 <- readChanFromSer
    case cmd2 of
      Right _ -> assert `failure` (side, cmd2)
      Left (CmdUpdateCli cmd) -> do
        cmdUpdateCli cmd
      Left (CmdQueryCli cmd) -> do
        a <- cmdQueryCli cmd
        writeChanToSer $ toDyn a
    loop

loopCli4 :: (MonadClientUI m, MonadClientChan m)
         => (CmdUpdateCli -> m ())
         -> (forall a. Typeable a => CmdQueryCli a -> m a)
         -> (CmdUpdateUI -> m ())
         -> (forall a. Typeable a => CmdQueryUI a -> m a)
         -> m ()
loopCli4 cmdUpdateCli cmdQueryCli cmdUpdateUI cmdQueryUI = do
  initCli
  loop
 where
  loop = do
    cmd4 <- readChanFromSer
    case cmd4 of
      Right (CmdUpdateUI cmd) -> do
        cmdUpdateUI cmd
      Right (CmdQueryUI cmd) -> do
        a <- cmdQueryUI cmd
        writeChanToSer $ toDyn a
      Left (CmdUpdateCli cmd) -> do
        cmdUpdateCli cmd
      Left (CmdQueryCli cmd) -> do
        a <- cmdQueryCli cmd
        writeChanToSer $ toDyn a
    loop
