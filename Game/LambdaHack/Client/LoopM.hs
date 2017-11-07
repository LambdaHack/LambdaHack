{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( loopCli
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.HandleResponseM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector

initAI :: MonadClient m => ClientOptions -> m ()
initAI sclientOptions = do
  modifyClient $ \cli -> cli {sclientOptions}
  side <- getsClient sside
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "initializing."

initUI :: MonadClientUI m => KeyKind -> Config -> ClientOptions -> m ()
initUI copsClient sconfig sclientOptions = do
  modifyClient $ \cli -> cli {sclientOptions}
  side <- getsClient sside
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "initializing."
  -- Start the frontend.
  schanF <- chanFrontend sclientOptions
  let !sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
      sess = emptySessionUI sconfig
  putSession sess { schanF
                  , sbinding
                  , sxhair = TVector $ Vector 1 1 }
                      -- a step south-east, less alarming

-- | The main game loop for an AI or UI client.
loopCli :: ( MonadClientSetup m
           , MonadClientUI m
--           , MonadClientAtomic m
           , MonadStateWrite m
           , MonadClientReadResponse m
           , MonadClientWriteRequest m )
        => KeyKind -> Config -> ClientOptions -> m ()
loopCli copsClient sconfig sclientOptions = do
  hasUI <- clientHasUI
  if not hasUI then initAI sclientOptions else initUI copsClient sconfig sclientOptions
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  restoredG <- tryRestore
  restored <- case restoredG of
    Just (cli, msess) | not $ snewGameCli sclientOptions -> do
      -- Restore game.
      schanF <- getsSession schanF
      sbinding <- getsSession sbinding
      maybe (return ()) (\sess ->
        putSession sess {schanF, sbinding, sconfig}) msess
      putClient cli {sclientOptions}
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
