{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopCli2, loopCli4) where

import Data.Dynamic
import Control.Monad
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

initCli :: MonadClientChan m => Bool -> (CmdUpdateCli -> m ()) -> m ()
initCli isAI cmdUpdateCli = do
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops@Kind.COps{corule} <- getsState scops
  configUI <- getsClient sconfigUI
  let pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
      title = rtitle $ Kind.stdRuleset corule
  restored <- restoreGame isAI configUI pathsDataFile title
  case restored of
    Right msg -> do  -- First visit ever, use the initial state.
      -- TODO: create or restore from config clients RNG seed
      msgAdd msg
      let expected cmd = case cmd of RestartCli{} -> True; _ -> False
      waitForCmd cmdUpdateCli expected
    Left (s, cli, msg) -> do  -- Restore a game or at least history.
      let sCops = updateCOps (const cops) s
      putState sCops
      putClient cli
      msgAdd msg
      let expected cmd = case cmd of ContinueSavedCli{} -> True; _ -> False
      waitForCmd cmdUpdateCli expected
  modifyState $ updateQuit (const Nothing)
  -- State and client state are now valid.

waitForCmd :: MonadClientChan m
           => (CmdUpdateCli -> m ()) -> (CmdUpdateCli -> Bool)
           -> m ()
waitForCmd cmdUpdateCli expected = do
  side <- getsState sside
  cmd1 <- readChanFromSer
  case cmd1 of
    Left (CmdUpdateCli cmd) | expected cmd -> cmdUpdateCli cmd
    _ -> assert `failure` (side, cmd1)

loopCli2 :: MonadClientChan m
         => (CmdUpdateCli -> m ())
         -> (forall a. Typeable a => CmdQueryCli a -> m a)
         -> m ()
loopCli2 cmdUpdateCli cmdQueryCli = do
  initCli True cmdUpdateCli
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
    quit <- getsState squit
    when (isNothing quit) loop

loopCli4 :: (MonadClientUI m, MonadClientChan m)
         => (CmdUpdateCli -> m ())
         -> (forall a. Typeable a => CmdQueryCli a -> m a)
         -> (CmdUpdateUI -> m ())
         -> (forall a. Typeable a => CmdQueryUI a -> m a)
         -> m ()
loopCli4 cmdUpdateCli cmdQueryCli cmdUpdateUI cmdQueryUI = do
  initCli False cmdUpdateCli
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
    quit <- getsState squit
    when (isNothing quit) loop
