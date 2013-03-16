{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of server commands.
-- See https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture.
module Game.LambdaHack.Server
  ( mainSer
  ) where

import Control.Monad.Writer.Strict (WriterT, execWriterT)
import System.Environment (getArgs)

import Game.LambdaHack.Action
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.Client
import Game.LambdaHack.Client.Action
import Game.LambdaHack.CmdCli
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdSerSem
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.LoopAction
import Game.LambdaHack.Server.State

-- | The semantics of server commands.
cmdSerSem :: MonadServer m => CmdSer -> m [Atomic]
cmdSerSem cmd = execWriterT $ cmdSerWriterT cmd

cmdSerWriterT :: MonadServer m => CmdSer -> WriterT [Atomic] m ()
cmdSerWriterT cmd = case cmd of
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  WaitSer aid -> waitSer aid
  PickupSer aid i k l -> pickupSer aid i k l
  DropSer aid iid -> dropSer aid iid
  ProjectSer aid p eps iid container -> projectSer aid p eps iid container
  ApplySer aid iid container -> applySer aid iid container
  TriggerSer aid p -> triggerSer aid p
  SetPathSer aid path -> setPathSer aid path
  GameRestartSer aid -> gameRestartSer aid
  GameExitSer _ -> gameExitSer
  GameSaveSer _ -> gameSaveSer
  CfgDumpSer aid -> cfgDumpSer aid

debugArgs :: IO DebugModeSer
debugArgs = do
  args <- getArgs
  let usage =
        [ "Configure server debug options here, gameplay in config.rules.ini."
        , "  --knowMap reveal map for all clients in the next game"
        , "  --knowEvents show all events in the next game (needs --knowMap)"
        , "  --sniffIn display all incoming commands on console "
        , "  --sniffOut display all outgoing commands on console "
        , "  --allClear let all map tiles be translucent"
        , "  --tryFov m set a Field of View mode, where m can be"
        , "    Digital r, r > 0"
        , "    Permissive"
        , "    Shadow"
        , "    Blind"
        ]
      parseArgs [] = defDebugModeSer
      parseArgs ("--knowMap" : rest) =
        (parseArgs rest) {sknowMap = True}
      parseArgs ("--knowEvents" : rest) =
        (parseArgs rest) {sknowEvents = True}
      parseArgs ("--sniffIn" : rest) =
        (parseArgs rest) {sniffIn = True}
      parseArgs ("--sniffOut" : rest) =
        (parseArgs rest) {sniffOut = True}
      parseArgs ("--allClear" : rest) =
        (parseArgs rest) {sallClear = True}
      parseArgs ("--tryFov" : "Digital" : r : rest) | (read r :: Int) > 0 =
        (parseArgs rest) {stryFov = Just $ Digital $ read r}
      parseArgs ("--tryFov" : fovMode : rest) =
        (parseArgs rest) {stryFov = Just $ read fovMode}
      parseArgs _ = error $ unlines usage
  return $! parseArgs args

-- | Fire up the frontend with the engine fueled by content.
-- The action monad types to be used are determined by the 'executorSer'
-- and 'executorCli' calls. If other functions are used in their place
-- the types are different and so the whole pattern of computation
-- is different. Which of the frontends is run depends on the flags supplied
-- when compiling the engine library.
mainSer :: Kind.COps -> IO ()
mainSer copsSlow = do
  sdebugNxt <- debugArgs
  let cops = speedupCOps False copsSlow
      loopClientUI :: ( MonadActionAbort m, MonadAction m
                      , MonadClientUI m, MonadClientConn CmdClientUI m )
                   => m ()
      loopClientUI = loopUI cmdClientUISem
      loopClientAI :: ( MonadAction m
                      , MonadClient m, MonadClientConn CmdClientAI m )
                   => m ()
      loopClientAI = loopAI cmdClientAISem
      exeClientUI = executorCli loopClientUI
      exeClientAI = executorCli loopClientAI
      loopServer = loopSer sdebugNxt cmdSerSem
      exeServer executorUI executorAI =
        executorSer (loopServer executorUI executorAI cops)
  exeFrontend cops exeClientUI exeClientAI exeServer
  waitForChildren
