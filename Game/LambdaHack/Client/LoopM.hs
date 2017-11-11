{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( loopCli
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.ClientOptions
import Game.LambdaHack.Client.HandleResponseM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Response
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector

initAI :: MonadClient m => ClientOptions -> m ()
initAI soptions = do
  modifyClient $ \cli -> cli {soptions}
  side <- getsClient sside
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "initializing."

initUI :: MonadClientUI m => KeyKind -> UIOptions -> ClientOptions -> m ()
initUI copsClient sUIOptions soptions = do
  modifyClient $ \cli -> cli {soptions}
  side <- getsClient sside
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "initializing."
  -- Start the frontend.
  schanF <- chanFrontend soptions
  let !sbinding = stdBinding copsClient sUIOptions
        -- evaluate to check for errors
      sess = emptySessionUI sUIOptions
  putSession sess { schanF
                  , sbinding
                  , sxhair = TVector $ Vector 1 1 }
                      -- a step south-east, less alarming

-- | The main game loop for an AI or UI client. It receives responses from
-- the server, changes internal client state accordingly, analyzes
-- ensuing human or AI commands and sends resulting requests to the server.
-- Depending on whether it's an AI or UI client,
-- it sends AI or human player requests.
loopCli :: ( MonadClientSetup m
           , MonadClientUI m
           , MonadClientAtomic m
           , MonadClientReadResponse m
           , MonadClientWriteRequest m )
        => KeyKind -> UIOptions -> ClientOptions -> m ()
loopCli copsClient sUIOptions soptions = do
  hasUI <- clientHasUI
  if not hasUI then initAI soptions else initUI copsClient sUIOptions soptions
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  restoredG <- tryRestore
  restored <- case restoredG of
    Just (cli, msess) | not $ snewGameCli soptions -> do
      -- Restore game.
      schanF <- getsSession schanF
      sbinding <- getsSession sbinding
      maybe (return ()) (\sess ->
        putSession sess {schanF, sbinding, sUIOptions}) msess
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
    (True, RespUpdAtomic s UpdResume{}) -> do
      let sCops = updateCOps (const cops) s
      handleResponse $ RespUpdAtomic sCops $ UpdResumeServer sCops
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
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponse cmd
    quit <- getsClient squit
    unless quit loop
