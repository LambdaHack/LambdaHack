{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( MonadClientReadResponse(..)
  , loopCli
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , initAI, initUI
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.ClientOptions
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.HandleResponseM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Vector

-- | Client monad in which one can receive responses from the server.
class MonadClient m => MonadClientReadResponse m where
  receiveResponse :: m Response

initAI :: MonadClient m => m ()
initAI = do
  side <- getsClient sside
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "initializing."

initUI :: (MonadClient m, MonadClientUI m) => CCUI -> m ()
initUI sccui@CCUI{coscreen} = do
  side <- getsClient sside
  soptions <- getsClient soptions
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "initializing."
  -- Start the frontend.
  schanF <- chanFrontend coscreen soptions
  modifySession $ \sess ->
    sess { schanF
         , sccui
         , sxhair = TVector $ Vector 0 0 }
             -- a step south-east, less alarming

-- | The main game loop for an AI or UI client. It receives responses from
-- the server, changes internal client state accordingly, analyzes
-- ensuing human or AI commands and sends resulting requests to the server.
-- Depending on whether it's an AI or UI client, it sends AI or human player
-- requests.
--
-- The loop is started in client state that is empty except for
-- the @sside@ and @seps@ fields, see 'emptyStateClient'.
loopCli :: ( MonadClientSetup m
           , MonadClientUI m
           , MonadClientAtomic m
           , MonadClientReadResponse m
           , MonadClientWriteRequest m )
        => CCUI -> UIOptions -> ClientOptions -> m ()
loopCli ccui sUIOptions soptions = do
  modifyClient $ \cli -> cli {soptions}
  hasUI <- clientHasUI
  if not hasUI then initAI else initUI ccui
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  restoredG <- tryRestore
  restored <- case restoredG of
    Just (cli, msess) | not $ snewGameCli soptions -> do
      -- Restore game.
      schanF <- getsSession schanF
      sccui <- getsSession sccui
      maybe (return ()) (\sess -> modifySession $ const
        sess {schanF, sccui, sUIOptions}) msess
      putClient cli {soptions}
      return True
    Just (_, msessR) -> do
      -- Preserve previous history, if any (--newGame).
      maybe (return ()) (\sessR -> modifySession $ \sess ->
        sess {shistory = shistory sessR}) msessR
      return False
    _ -> return False
  side <- getsClient sside
  cmd1 <- receiveResponse
  case (restored, cmd1) of
    (True, RespUpdAtomic _ UpdResume{}) -> return ()
    (True, RespUpdAtomic _ UpdRestart{}) ->
      when hasUI $ msgAdd "Ignoring an old savefile and starting a new game."
    (False, RespUpdAtomic _ UpdResume{}) ->
      error $ "Savefile of client " ++ show side ++ " not usable."
              `showFailure` ()
    (False, RespUpdAtomic _ UpdRestart{}) -> return ()
    (True, RespUpdAtomicNoState UpdResume{}) -> undefined
    (True, RespUpdAtomicNoState UpdRestart{}) ->
      when hasUI $ msgAdd "Ignoring an old savefile and starting a new game."
    (False, RespUpdAtomicNoState UpdResume{}) ->
      error $ "Savefile of client " ++ show side ++ " not usable."
              `showFailure` ()
    (False, RespUpdAtomicNoState UpdRestart{}) -> return ()
    _ -> error $ "unexpected command" `showFailure` (side, restored, cmd1)
  handleResponse cmd1
  -- State and client state now valid.
  let cliendKindText = if not hasUI then "AI" else "UI"
  debugPossiblyPrint $ cliendKindText <+> "client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ cliendKindText <+> "client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponse cmd
    quit <- getsClient squit
    unless quit loop
