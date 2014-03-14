{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( exeFrontend
  , MonadClient, MonadClientUI, MonadClientReadServer, MonadClientWriteServer
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Maybe

import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.AtomicSemCli
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.ClientSem
import Game.LambdaHack.Client.ConfigUI
import Game.LambdaHack.Client.LoopAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Animation (DebugModeCli (..))
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Frontend

storeUndo :: MonadClient m => Atomic -> m ()
storeUndo _atomic =
  maybe skip (\a -> modifyClient $ \cli -> cli {sundo = a : sundo cli})
    $ Nothing   -- TODO: undoAtomic atomic

handleResponseAI :: (MonadAtomic m, MonadClientWriteServer RequestTimed m)
                 => ResponseAI -> m ()
handleResponseAI cmd = case cmd of
  RespCmdAtomicAI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ (\c -> cmdAtomicSemCli c
                 >> execCmdAtomic c) cmds
    mapM_ (storeUndo . CmdAtomic) cmds
  RespQueryAI aid -> do
    cmdC <- queryAI aid
    writeServer cmdC
  RespPingAI -> writeServer $ ReqPongHack []

handleResponseUI :: ( MonadAtomic m, MonadClientUI m
                    , MonadClientWriteServer Request m )
                 => ResponseUI -> m ()
handleResponseUI cmd = case cmd of
  RespCmdAtomicUI cmdA -> do
    cmds <- cmdAtomicFilterCli cmdA
    mapM_ (\c -> cmdAtomicSemCli c
                 >> execCmdAtomic c
                 >> drawRespCmdAtomicUI False c) cmds
    mapM_ (storeUndo . CmdAtomic) cmds  -- TODO: only store cmdA?
  RespSfxAtomicUI sfx -> do
    drawRespSfxAtomicUI False sfx
    storeUndo $ SfxAtomic sfx
  RespQueryUI aid -> do
    mleader <- getsClient _sleader
    assert (isJust mleader `blame` "query without leader" `twith` cmd) skip
    cmdH <- queryUI aid
    writeServer cmdH
  RespPingUI -> pongUI

-- | Wire together game content, the main loop of game clients,
-- the main game loop assigned to this frontend (possibly containing
-- the server loop, if the whole game runs in one process),
-- UI config and the definitions of game commands.
exeFrontend :: ( MonadAtomic m, MonadClientUI m
               , MonadClientReadServer ResponseUI m
               , MonadClientWriteServer Request m
               , MonadAtomic n
               , MonadClientReadServer ResponseAI n
               , MonadClientWriteServer RequestTimed n )
            => (m () -> SessionUI -> State -> StateClient
                -> ChanServer ResponseUI Request
                -> IO ())
            -> (n () -> SessionUI -> State -> StateClient
                -> ChanServer ResponseAI RequestTimed
                -> IO ())
            -> Kind.COps -> DebugModeCli
            -> ((FactionId -> ChanFrontend -> ChanServer ResponseUI Request
                 -> IO ())
               -> (FactionId -> ChanServer ResponseAI RequestTimed
                   -> IO ())
               -> IO ())
            -> IO ()
exeFrontend executorUI executorAI
            cops@Kind.COps{corule} sdebugCli exeServer = do
  -- UI config reloaded at each client start.
  sconfigUI <- mkConfigUI corule
  let stdRuleset = Kind.stdRuleset corule
      !sbinding = stdBinding corule sconfigUI  -- evaluate to check for errors
      sdebugMode =
        (\dbg -> dbg {sfont =
            sfont dbg `mplus` Just (configFont sconfigUI)}) .
        (\dbg -> dbg {smaxFps =
            smaxFps dbg `mplus` Just (configMaxFps sconfigUI)}) .
        (\dbg -> dbg {snoAnim =
            snoAnim dbg `mplus` Just (configNoAnim sconfigUI)}) .
        (\dbg -> dbg {ssavePrefixCli =
            ssavePrefixCli dbg `mplus` Just (rsavePrefix stdRuleset)})
        $ sdebugCli
  defHist <- defHistory
  let exeClientUI = executorUI $ loopUI sdebugMode handleResponseUI
      exeClientAI = executorAI $ loopAI sdebugMode handleResponseAI
      cli = defStateClient defHist sconfigUI
      s = updateCOps (const cops) emptyState
      eClientAI fid =
        let noSession = assert `failure` "AI client needs no UI session"
                               `twith` fid
        in exeClientAI noSession s (cli fid True)
      eClientUI sescMVar fid fromF =
        let sfconn = connFrontend fid fromF
        in exeClientUI SessionUI{..} s (cli fid False)
  startupF sdebugMode $ \sescMVar -> exeServer (eClientUI sescMVar) eClientAI
