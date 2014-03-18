-- | Startup up the frontend together with the server, which starts up clients.
module Game.LambdaHack.Client.UI.StartupFrontendClient
  ( srtFrontend
  ) where

import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Exception.Assert.Sugar

import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.State

-- | Wire together game content, the main loop of game clients,
-- the main game loop assigned to this frontend (possibly containing
-- the server loop, if the whole game runs in one process),
-- UI config and the definitions of game commands.
srtFrontend :: (DebugModeCli -> SessionUI -> State -> StateClient
                -> chanServerUI
                -> IO ())
            -> (DebugModeCli -> SessionUI -> State -> StateClient
                -> chanServerAI
                -> IO ())
            -> KeyKind -> Kind.COps -> DebugModeCli
            -> ((FactionId -> chanServerUI -> IO ())
               -> (FactionId -> chanServerAI -> IO ())
               -> IO ())
            -> IO ()
srtFrontend executorUI executorAI
            copsClient cops@Kind.COps{corule} sdebugCli exeServer = do
  -- UI config reloaded at each client start.
  sconfig <- mkConfig corule
  let !sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
      sdebugMode = applyConfigToDebug sconfig sdebugCli corule
  defaultHist <- defaultHistory
  let cli = defStateClient defaultHist
      s = updateCOps (const cops) emptyState
      exeClientAI fid =
        let noSession = assert `failure` "AI client needs no UI session"
                               `twith` fid
        in executorAI sdebugMode noSession s (cli fid True)
      exeClientUI sescMVar loopFrontend fid chanServerUI = do
        responseF <- STM.newTQueueIO
        requestF <- STM.newTQueueIO
        let schanF = ChanFrontend{..}
        a <- async $ loopFrontend schanF
        link a
        executorUI sdebugMode SessionUI{..} s (cli fid False) chanServerUI
        STM.atomically $ STM.writeTQueue requestF FrontFinish
        wait a
  -- TODO: let each client start his own raw frontend (e.g., gtk, though
  -- that leads to disaster); then don't give server as the argument
  -- to startupF, but the Client.hs (when it ends, gtk ends); server is
  -- then forked separately and client doesn't need to know about
  -- starting servers.
  startupF sdebugMode $ \sescMVar loopFrontend ->
    exeServer (exeClientUI sescMVar loopFrontend) exeClientAI
