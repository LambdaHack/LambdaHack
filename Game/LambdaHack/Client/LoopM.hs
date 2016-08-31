{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopM
  ( loopAI, loopUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.HandleResponseM
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolM
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind

-- | The main game loop for an AI client.
loopAI :: ( MonadClientSetup m
          , MonadAtomic m
          , MonadClientReadResponse ResponseAI m
          , MonadClientWriteRequest RequestAI m )
       => DebugModeCli -> m ()
loopAI sdebugCli = do
  modifyClient $ \cli -> cli {sisAI = True}
  side <- getsClient sside
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  modifyClient $ \cli -> cli {sdebugCli}
  restoredG <- tryRestore
  restored <- case restoredG of
    Just (s, cli, ()) | not $ snewGameCli sdebugCli -> do  -- Restore game.
      let sCops = updateCOps (const cops) s
      handleSelfAI $ UpdResumeServer sCops
      putClient cli {sdebugCli}
      return True
    _ -> return False
  cmd1 <- receiveResponse
  case (restored, cmd1) of
    (True, RespUpdAtomicAI UpdResume{}) -> return ()
    (True, RespUpdAtomicAI UpdRestart{}) -> return ()
    (False, RespUpdAtomicAI UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomicAI UpdRestart{}) -> return ()
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  handleResponseAI cmd1
  -- State and client state now valid.
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "AI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponseAI cmd
    quit <- getsClient squit
    unless quit loop

-- | The main game loop for a UI client.
loopUI :: ( MonadClientSetup m
          , MonadClientUI m
          , MonadAtomic m
          , MonadClientReadResponse ResponseUI m
          , MonadClientWriteRequest RequestUI m )
       => KeyKind -> Config -> DebugModeCli -> m ()
loopUI copsClient sconfig sdebugCli = do
  modifyClient $ \cli ->
    cli { sisAI = False
        , sxhair = TVector $ Vector 1 1 }  -- a step south-east, less alarming
  -- Start the frontend.
  schanF <- chanFrontend sdebugCli
  let !sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  modifyClient $ \cli -> cli {sdebugCli}
  restoredG <- tryRestore
  restored <- case restoredG of
    Just (s, cli, sess) | not $ snewGameCli sdebugCli -> do  -- Restore game.
      let sCops = updateCOps (const cops) s
      handleSelfUI $ UpdResumeServer sCops
      putSession sess {schanF, sbinding}
      putClient cli {sdebugCli}
      return True
    _ -> do  -- First visit ever, use the initial state.
      -- But preserve the previous history, if any (--newGame).
      let sess = emptySessionUI sconfig
      case restoredG of
        Just (_, _, sessR) ->
          putSession sess {schanF, sbinding, shistory = shistory sessR}
        Nothing ->
          putSession sess {schanF, sbinding}
      return False
  resetSessionStart
  Kind.COps{corule} <- getsState scops
  let title = rtitle $ Kind.stdRuleset corule
  side <- getsClient sside
  cmd1 <- receiveResponse
  case (restored, cmd1) of
    (True, RespUpdAtomicUI UpdResume{}) -> return ()
    (True, RespUpdAtomicUI UpdRestart{}) -> do
      msgAdd $
        "Ignoring an old savefile and starting a new" <+> title <+> "game."
    (False, RespUpdAtomicUI UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomicUI UpdRestart{}) -> do
      msgAdd $ "Welcome to" <+> title <> "!"
      -- Generate initial history. Only for UI clients.
      shistory <- defaultHistory $ configHistoryMax sconfig
      modifySession $ \sess -> sess {shistory}
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  handleResponseUI cmd1
  fact <- getsState $ (EM.! side) . sfactionD
  if isAIFact fact then do
    -- Prod the frontend to flush frames and start showing then continuously.
    slides <- reportToSlideshow []
    void $ getConfirms ColorFull [K.spaceKM, K.escKM] slides
  else do
    mode <- getGameMode
    promptAdd $ mdesc mode <+> "Are you up for the challenge?"
    slides <- reportToSlideshow [K.spaceKM, K.escKM]
    km <- getConfirms ColorFull [K.spaceKM, K.escKM] slides
    if km == K.escKM then addPressedEsc else promptAdd "Prove yourself!"
  -- State and client state now valid.
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPossiblyPrint $ "Frontend" <+> tshow side <+> "shutting down."
  frontendShutdown
  debugPossiblyPrint $ "UI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponseUI cmd
    quit <- getsClient squit
    unless quit loop
