-- | Semantics of server commands.
module Game.LambdaHack.Server
  ( cmdSerSem
  , loopSer, executorSer, waitForChildren, speedupCOps
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdAtomic
import Game.LambdaHack.Server.CmdAtomicSem
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.LoopAction

-- | The semantics of server commands.
cmdSerSem :: (MonadAction m, MonadServerChan m) => CmdSer -> m ()
cmdSerSem cmd = do
  cmds <- execWriterT $ cmdSerWriterT cmd
  mapM_ cmdAtomicSem cmds

cmdSerWriterT :: MonadServerChan m => CmdSer -> WriterT [CmdAtomic] m ()
cmdSerWriterT cmd = case cmd of
  ApplySer aid v iid container -> applySer aid v iid container
  ProjectSer aid p eps v iid container -> projectSer aid p eps v iid container
  TriggerSer aid p -> triggerSer aid p
  PickupSer aid i k l -> pickupSer aid i k l
  DropSer aid item -> dropSer aid item
  WaitSer aid -> waitSer aid
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  GameExitSer -> undefined -- gameExitSer
  GameRestartSer -> undefined -- gameRestartSer
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer
  DirToAction actor allowAttacks dir -> do
-- TODO: move this to ClientState
--    modifyState $ updateActorBody actor $ \ m -> m { bdirAI = Just (dir, 0) }
    if allowAttacks
      then moveSer actor dir
      else runSer actor dir
  ClearPath aid -> do
    fromPath <- getsState $ bpath . getActorBody aid
    tell [AlterPath aid fromPath Nothing]
  FollowPath aid dir path darken -> do
    fromPath <- getsState $ bpath . getActorBody aid
    tell [AlterPath aid fromPath (Just path)]
    when darken $ do
      fromColor <- getsState $ bcolor . getActorBody aid
      tell [ColorActor aid fromColor (Just Color.BrBlack)]
    moveSer aid dir
  DieSer aid -> do  -- TODO: explode if a potion
--    bitems <- getsState $ getActorBag actor
--    Actor{bpos} <- getsState (getActorBody actor)
--    modifyState (updateArena (dropItemsAt bitems bpos))
    body <- getsState $ getActorBody aid
    tell [KillAtomic aid body]
