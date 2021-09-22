{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( MonadClientReadResponse(..)
  , loopCli
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , initAI, initUI, loopAI, minimalDelay, loopUI
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.Concurrent

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.HandleResponseM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Request
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Common.ClientOptions

-- | Client monad in which one can receive responses from the server.
class MonadClient m => MonadClientReadResponse m where
  receiveResponse :: m Response
  tryReceiveResponse :: m (Maybe Response)

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
  modifySession $ \sess -> sess {schanF, sccui}

-- | The main game loop for an AI or UI client. It receives responses from
-- the server, changes internal client state accordingly, analyzes
-- ensuing human or AI commands and sends resulting requests to the server.
-- Depending on whether it's an AI or UI client, it sends AI or human player
-- requests.
--
-- The loop is started in client state that is empty except for
-- the @sside@ and @seps@ fields, see 'emptyStateClient'.
loopCli :: forall m. ( MonadClientSetup m
                     , MonadClientUI m
                     , MonadClientAtomic m
                     , MonadClientReadResponse m
                     , MonadClientWriteRequest m )
        => CCUI -> UIOptions -> ClientOptions -> m ()
loopCli ccui sUIOptions clientOptions = do
  modifyClient $ \cli -> cli {soptions = clientOptions}
  side <- getsClient sside
  hasUI <- clientHasUI
  if not hasUI then initAI else initUI ccui
  let cliendKindText = if not hasUI then "AI" else "UI"
  debugPossiblyPrint $ cliendKindText <+> "client"
                       <+> tshow side <+> "started 1/4."
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  restoredG <- tryRestore
  restored <- case restoredG of
    Just (cli, msess) | not $ snewGameCli clientOptions -> do
      -- Restore game.
      schanF <- getsSession schanF
      sccui <- getsSession sccui
      maybe (return ()) (\sess -> modifySession $ const
        sess {schanF, sccui, sUIOptions}) msess
      let noAnim = fromMaybe False $ snoAnim $ soptions cli
      putClient cli {soptions = clientOptions {snoAnim = Just noAnim}}
      return True
    Just (_, msessR) -> do
      -- Preserve previous history, if any.
      maybe (return ()) (\sessR -> modifySession $ \sess ->
        sess {shistory = shistory sessR}) msessR
      return False
    _ -> return False
  debugPossiblyPrint $ cliendKindText <+> "client"
                       <+> tshow side <+> "started 2/4."
  -- At this point @ClientState@ not overriten dumbly and @State@ valid.
  tabA <- createTabBFS
  tabB <- createTabBFS
  modifyClient $ \cli -> cli {stabs = (tabA, tabB)}
  cmd1 <- receiveResponse
  debugPossiblyPrint $ cliendKindText <+> "client"
                       <+> tshow side <+> "started 3/4."
  case (restored, cmd1) of
    (True, RespUpdAtomic _ UpdResume{}) -> return ()
    (True, RespUpdAtomic _ UpdRestart{}) ->
      when hasUI $
        clientPrintUI "Ignoring an old savefile and starting a new game."
    (False, RespUpdAtomic _ UpdResume{}) ->
      error $ "Savefile of client " ++ show side ++ " not usable."
              `showFailure` ()
    (False, RespUpdAtomic _ UpdRestart{}) -> return ()
    (True, RespUpdAtomicNoState UpdResume{}) -> undefined
    (True, RespUpdAtomicNoState UpdRestart{}) ->
      when hasUI $
        clientPrintUI "Ignoring an old savefile and starting a new game."
    (False, RespUpdAtomicNoState UpdResume{}) ->
      error $ "Savefile of client " ++ show side ++ " not usable."
              `showFailure` ()
    (False, RespUpdAtomicNoState UpdRestart{}) -> return ()
    _ -> error $ "unexpected command" `showFailure` (side, restored, cmd1)
  handleResponse cmd1
  -- State and client state now valid.
  debugPossiblyPrint $ cliendKindText <+> "client"
                       <+> tshow side <+> "started 4/4."
  if hasUI
  then loopUI minimalDelay Nothing
  else loopAI
  side2 <- getsClient sside
  debugPossiblyPrint $ cliendKindText <+> "client" <+> tshow side2
                       <+> "(initially" <+> tshow side <> ") stopped."

loopAI :: forall m. ( MonadClientSetup m
                    , MonadClientUI m
                    , MonadClientAtomic m
                    , MonadClientReadResponse m
                    , MonadClientWriteRequest m )
       => m ()
loopAI = do
  cmd <- receiveResponse
  handleResponse cmd
  quit <- getsClient squit
  unless quit
    loopAI

-- | @100@ times that is 60 polls per second, which feels snappy.
minimalDelay :: Int
minimalDelay = 150

loopUI :: forall m. ( MonadClientSetup m
                    , MonadClientUI m
                    , MonadClientAtomic m
                    , MonadClientReadResponse m
                    , MonadClientWriteRequest m )
       => Int -> Maybe RequestUI -> m ()
loopUI pollingDelay mreq = do
  let smallDelay = 10 * minimalDelay
      longestDelay = 10 * smallDelay
  mcmd <- tryReceiveResponse
  case mcmd of
    Just cmd -> do
      handleResponse cmd
      -- @squit@ can be changed only in @handleResponse@, so this is the only
      -- place where it needs to be checked.
      quit <- getsClient squit
      unless quit $
        loopUI minimalDelay mreq  -- shortcut
    Nothing -> do
      let query = do
            mreqNew <- queryUI
            return (True, mreqNew)
      (progress, mreq2) <- do
        sreqExpected <- getsSession sreqExpected
        case mreq of
          Nothing | sreqExpected ->
            query
          Nothing | pollingDelay > smallDelay -> do
            msgAdd MsgActionAlert "Server has not updated reality in time. Regardless, making UI accessible. Press ESC to listen to server some more."
            query
          Just req | sreqExpected -> do
            modifySession $ \sess -> sess {sreqExpected = False}
            sendRequestUI req  -- old req sent and not needed any more
            return (True, Nothing)
          Just{} | pollingDelay == longestDelay -> do
            msgAdd MsgActionAlert "Server not ready in time. Cancelling the action. Issue a new one."
            query  -- old req dropped
          _ -> return (False, mreq)
      if progress then
        loopUI minimalDelay mreq2
      else do
        liftIO $ threadDelay pollingDelay
        loopUI (min longestDelay $ 2 * pollingDelay) mreq2
