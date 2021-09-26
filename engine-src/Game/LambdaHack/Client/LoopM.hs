{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( MonadClientReadResponse(..)
  , loopCli
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , initAI, initUI, loopAI, longestDelay, loopUIwithResetTimeout, loopUI
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.Time.Clock.POSIX

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.HandleAtomicM
import Game.LambdaHack.Client.HandleResponseM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

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
  modifySession $ \sess -> sess {schanF, sccui}

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
  then loopUIwithResetTimeout
  else loopAI
  side2 <- getsClient sside
  debugPossiblyPrint $ cliendKindText <+> "client" <+> tshow side2
                       <+> "(initially" <+> tshow side <> ") stopped."

loopAI :: ( MonadClientSetup m
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

-- | Alarm after this many seconds without server querying us for a command.
longestDelay :: Int
longestDelay = 1  -- rather high to accomodate slow browsers

loopUIwithResetTimeout :: ( MonadClientSetup m
                          , MonadClientUI m
                          , MonadClientAtomic m
                          , MonadClientReadResponse m
                          , MonadClientWriteRequest m )
                       => m ()
loopUIwithResetTimeout = do
  current <- liftIO getPOSIXTime
  loopUI current

-- | The argument is the time of last UI query from the server.
-- After @longestDelay@ seconds past this date, the client considers itself
-- ignored and displays a warning and, at a keypress, gives
-- direct control to the player, no longer waiting for the server
-- to prompt it to do so.
loopUI :: ( MonadClientSetup m
          , MonadClientUI m
          , MonadClientAtomic m
          , MonadClientReadResponse m
          , MonadClientWriteRequest m )
       => POSIXTime -> m ()
loopUI timeOfLastQuery = do
  sreqPending <- getsSession sreqPending
  alarm <- elapsedSessionTimeGT timeOfLastQuery longestDelay
  if not alarm then do
    cmd <- receiveResponse
    handleResponse cmd
    -- @squit@ can be changed only in @handleResponse@, so this is the only
    -- place where it needs to be checked.
    quit <- getsClient squit
    unless quit $ case cmd of
      RespQueryUI -> loopUIwithResetTimeout
      _ -> do
        when (isJust sreqPending) $ do
          msgAdd MsgActionAlert "Warning: server updated game state after current command was issued by the client but before it was received by the server."
        loopUI timeOfLastQuery
  else do  -- timeout reached
    sregainControl <- getsSession sregainControl
    -- Ignore keypress if will be handled by special AI control regain case.
    keyPressed <- if sregainControl then return False else anyKeyPressed
    if keyPressed || isJust sreqPending then do
      -- The key pressed to gain control is not considered a command.
      discardPressedKey
      -- Special case for UI under AI control, because the default
      -- behaviour is too alarming for the player, especially during
      -- the insert coin demo before game is started.
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      if isAIFact fact && fleaderMode (gplayer fact) /= LeaderNull then
        -- Mark for immediate control regain from AI.
        modifySession $ \sess -> sess {sregainControl = True}
      else do
        -- Stop displaying the prompt, if any, but keep UI simple.
        modifySession $ \sess -> sess {sreqDelay = ReqDelayHandled}
        let msg = if isNothing sreqPending
                  then "Server delayed asking us for a command. Regardless, UI is made accessible. Press ESC twice to listen to server some more."
                  else "Server delayed receiving a command from us. The command is cancelled. Issue a new one."
        msgAdd MsgActionAlert msg
        mreqNew <- queryUI
        -- TODO: once this is really used, verify that if a request
        -- overwritten, nothing breaks due to some things in our ClientState
        -- and SessionUI (but fortunately not in State nor ServerState)
        -- already set as if it was performed.
        modifySession $ \sess -> sess {sreqPending = mreqNew}
        -- Relax completely.
        modifySession $ \sess -> sess {sreqDelay = ReqDelayNot}
      -- We may yet not know if server is ready, but perhaps server
      -- tried hard to contact us while we took control and now it sleeps
      -- for a bit, so let's give it the benefit of the doubt
      -- and a slight pause before we alarm the player again.
      loopUIwithResetTimeout
    else do
      -- We know server is not ready.
      modifySession $ \sess -> sess {sreqDelay = ReqDelayAlarm}
      -- We take a slight pause during which we display encouragement
      -- to press a key and receive game state changes and after which
      -- we check @keyPressed@ (which is cumulative) again.
      loopUIwithResetTimeout
