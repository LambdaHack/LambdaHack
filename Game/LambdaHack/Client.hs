{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of client commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( exeFrontend
  , MonadClient, MonadClientUI, MonadClientReadResponse, MonadClientWriteRequest
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.HandleResponseClient
import Game.LambdaHack.Client.LoopClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.MonadClientUI
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Common.Animation (DebugModeCli (..))
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Frontend

-- | Wire together game content, the main loop of game clients,
-- the main game loop assigned to this frontend (possibly containing
-- the server loop, if the whole game runs in one process),
-- UI config and the definitions of game commands.
exeFrontend :: ( MonadAtomic m, MonadClientUI m
               , MonadClientReadResponse ResponseUI m
               , MonadClientWriteRequest Request m
               , MonadAtomic n
               , MonadClientReadResponse ResponseAI n
               , MonadClientWriteRequest RequestTimed n )
            => (m () -> SessionUI -> State -> StateClient
                -> chanServerUI
                -> IO ())
            -> (n () -> SessionUI -> State -> StateClient
                -> chanServerAI
                -> IO ())
            -> Kind.COps -> DebugModeCli
            -> ((FactionId -> ChanFrontend -> chanServerUI
                 -> IO ())
               -> (FactionId -> chanServerAI
                   -> IO ())
               -> IO ())
            -> IO ()
exeFrontend executorUI executorAI
            cops@Kind.COps{corule} sdebugCli exeServer = do
  -- UI config reloaded at each client start.
  sconfigUI <- mkConfig corule
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
