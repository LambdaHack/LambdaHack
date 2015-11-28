{-# LANGUAGE RankNTypes #-}
-- | Startup up the frontend together with the server, which starts up clients.
module Game.LambdaHack.Client.UI.StartupFrontendClient
  ( srtFrontend
  ) where

import Control.Concurrent

import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.State

-- | Wire together game content, the main loops of game clients,
-- the main game loop assigned to this frontend (possibly containing
-- the server loop, if the whole game runs in one process),
-- UI config and the definitions of game commands.
srtFrontend :: forall chanServerUI chanServerAI.
               (SessionUI -> DebugModeCli -> State -> StateClient
                -> chanServerUI
                -> IO ())    -- ^ UI main loop
            -> (DebugModeCli -> State -> StateClient
                -> chanServerAI
                -> IO ())    -- ^ AI main loop
            -> KeyKind       -- ^ key and command content
            -> Kind.COps     -- ^ game content
            -> DebugModeCli  -- ^ client debug parameters
            -> ((FactionId -> chanServerUI -> IO ())
               -> (FactionId -> chanServerAI -> IO ())
               -> IO ())     -- ^ frontend main loop
            -> IO ()
srtFrontend executorUI executorAI
            copsClient cops sdebugCli exeServer = do
  -- UI config reloaded at each client start.
  sconfig <- mkConfig cops
  let !sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
      sdebugMode = applyConfigToDebug sconfig sdebugCli cops
  defaultHist <- defaultHistory $ configHistoryMax sconfig
  let cli = defStateClient defaultHist emptyReport
      s = updateCOps (const cops) emptyState
      exeClientAI fid = executorAI sdebugMode s (cli fid True)
      exeClientUI :: Maybe (MVar ())
                  -> ChanFrontend
                  -> FactionId
                  -> chanServerUI
                  -> IO ()
      exeClientUI sescMVar schanF fid chanServerUI = do
        executorUI SessionUI{..} sdebugMode s (cli fid False) chanServerUI
  -- TODO: let each client start his own raw frontend (e.g., gtk, though
  -- that leads to disaster); then don't give server as the argument
  -- to startupF, but the Client.hs (when it ends, gtk ends); server is
  -- then forked separately and client doesn't need to know about
  -- starting servers.
  startupF sdebugMode $ \sescMVar schanF ->
    exeServer (exeClientUI sescMVar schanF) exeClientAI
