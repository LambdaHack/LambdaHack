{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopAction (loopAI, loopUI) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind

initCli :: MonadClient m => DebugModeCli -> (State -> m ()) -> m Bool
initCli sdebugCli putSt = do
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  sconfigUI <- getsClient sconfigUI  -- config from file, not savegame
  modifyClient $ \cli -> cli {sdebugCli}
  restored <- restoreGame
  case restored of
    Just (s, cli) | not $ snewGameCli sdebugCli -> do  -- Restore the game.
      let sCops = updateCOps (const cops) s
      putSt sCops
      putClient cli {sdebugCli, sconfigUI}
      return True
    _ ->  -- First visit ever, use the initial state.
      return False

loopAI :: (MonadClientReadServer ResponseAI m)
       => DebugModeCli -> (ResponseAI -> m ()) -> m ()
loopAI sdebugCli cmdClientAISem = do
  side <- getsClient sside
  restored <- initCli sdebugCli
              $ \s -> cmdClientAISem $ RespCmdAtomicAI $ ResumeServerA s
  cmd1 <- readServer
  case (restored, cmd1) of
    (True, RespCmdAtomicAI ResumeA{}) -> return ()
    (True, RespCmdAtomicAI RestartA{}) -> return ()
    (False, RespCmdAtomicAI ResumeA{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespCmdAtomicAI RestartA{}) -> return ()
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  cmdClientAISem cmd1
  -- State and client state now valid.
  debugPrint $ "AI client" <+> tshow side <+> "started."
  loop
  debugPrint $ "AI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- readServer
    cmdClientAISem cmd
    quit <- getsClient squit
    unless quit loop

loopUI :: (MonadClientUI m, MonadClientReadServer ResponseUI m)
       => DebugModeCli -> (ResponseUI -> m ()) -> m ()
loopUI sdebugCli cmdClientUISem = do
  Kind.COps{corule} <- getsState scops
  let title = rtitle $ Kind.stdRuleset corule
  side <- getsClient sside
  restored <- initCli sdebugCli
              $ \s -> cmdClientUISem $ RespCmdAtomicUI $ ResumeServerA s
  cmd1 <- readServer
  msg <- case (restored, cmd1) of
    (True, RespCmdAtomicUI ResumeA{}) -> do
      cmdClientUISem cmd1
      return $! "Welcome back to" <+> title <> "."
    (True, RespCmdAtomicUI RestartA{}) -> do
      cmdClientUISem cmd1
      return $! "Starting a new" <+> title <+> "game."  -- ignore old savefile
    (False, RespCmdAtomicUI ResumeA{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespCmdAtomicUI RestartA{}) -> do
      cmdClientUISem cmd1
      return $! "Welcome to" <+> title <> "!"
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  fact <- getsState $ (EM.! side) . sfactionD
  if playerAiLeader $ gplayer fact then
    -- Prod the frontend to flush frames and start showing then continuously.
    void $ displayMore ColorFull "The team is under AI control (ESC to stop)."
  else
    msgAdd msg
  -- State and client state now valid.
  debugPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPrint $ "UI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- readServer
    cmdClientUISem cmd
    quit <- getsClient squit
    unless quit loop
