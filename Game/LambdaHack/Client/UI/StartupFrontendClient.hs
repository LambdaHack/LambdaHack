{-# LANGUAGE RankNTypes #-}
-- | Startup up the frontend together with the server, which starts up clients.
module Game.LambdaHack.Client.UI.StartupFrontendClient
  ( srtFrontend
  ) where

import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Frontend
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
               (Config -> RawFrontend -> DebugModeCli -> State -> StateClient
                -> chanServerUI
                -> IO ())    -- ^ UI main loop
            -> (DebugModeCli -> State -> StateClient
                -> chanServerAI
                -> IO ())    -- ^ AI main loop
            -> Kind.COps     -- ^ game content
            -> DebugModeCli  -- ^ client debug parameters
            -> IO ( FactionId -> chanServerUI -> IO ()
                  , FactionId -> chanServerAI -> IO () )
srtFrontend executorUI executorAI cops sdebugCli = do
  -- UI config reloaded at each client start.
  sconfig <- mkConfig cops
  let sdebugMode = applyConfigToDebug sconfig sdebugCli cops
  defaultHist <- defaultHistory $ configHistoryMax sconfig
  let cli = defStateClient defaultHist emptyReport
      s = updateCOps (const cops) emptyState
      exeClientAI fid = executorAI sdebugMode s (cli fid True)
      exeClientUI :: FactionId -> chanServerUI -> IO ()
      exeClientUI fid chanServerUI =
        startupF sdebugMode
        $ \fs -> executorUI sconfig fs sdebugMode s (cli fid False) chanServerUI
  return (exeClientUI, exeClientAI)
