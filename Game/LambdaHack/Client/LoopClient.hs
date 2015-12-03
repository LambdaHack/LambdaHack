{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopClient (loopAI, loopUI) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.HandleResponseClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind

initCli :: MonadClient m => DebugModeCli -> (State -> m ()) -> m Bool
initCli sdebugCli putSt = do
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  modifyClient $ \cli -> cli {sdebugCli}
  restored <- restoreGame
  case restored of
    Just (s, cli) | not $ snewGameCli sdebugCli -> do  -- Restore the game.
      let sCops = updateCOps (const cops) s
      putSt sCops
      putClient cli {sdebugCli}
      return True
    _ -> do  -- First visit ever, use the initial state.
      -- But preserve the previous history, if any (--newGame).
      case restored of
        Just (_, cliR) -> modifyClient $ \cli -> cli {shistory = shistory cliR}
        Nothing -> return ()
      return False

-- | The main game loop for an AI client.
loopAI :: ( MonadAtomic m
          , MonadClientReadResponse ResponseAI m
          , MonadClientWriteRequest RequestAI m )
       => DebugModeCli -> m ()
loopAI sdebugCli = do
  modifyClient $ \cli -> cli {sisAI = True}
  side <- getsClient sside
  restored <- initCli sdebugCli
              $ \s -> handleResponseAI $ RespUpdAtomicAI $ UpdResumeServer s
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
  debugPrint $ "AI client" <+> tshow side <+> "started."
  loop
  debugPrint $ "AI client" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponseAI cmd
    quit <- getsClient squit
    unless quit loop

-- | The main game loop for a UI client.
loopUI :: ( MonadClientUI m
          , MonadAtomic m
          , MonadClientReadResponse ResponseUI m
          , MonadClientWriteRequest RequestUI m )
       => KeyKind -> Config -> DebugModeCli -> m ()
loopUI copsClient sconfig sdebugCli = do
  modifyClient $ \cli ->
    cli { sisAI = False
        , scursor = TVector $ Vector 1 1 }  -- a step south-east, less alarming
  -- Start the frontend.
  rf <- liftIO $ startupF sdebugCli
  let !sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
      sescPressed = fescPressed rf
      schanF :: ChanFrontend
      schanF = chanFrontend rf
  putSession SessionUI{..}
  Kind.COps{corule} <- getsState scops
  let title = rtitle $ Kind.stdRuleset corule
  side <- getsClient sside
  restored <- initCli sdebugCli
              $ \s -> handleResponseUI $ RespUpdAtomicUI $ UpdResumeServer s
  cmd1 <- receiveResponse
  case (restored, cmd1) of
    (True, RespUpdAtomicUI UpdResume{}) -> do
      mode <- getGameMode
      msgAdd $ mdesc mode  -- the long description of the mode
      handleResponseUI cmd1
    (True, RespUpdAtomicUI UpdRestart{}) -> do
      msgAdd $
        "Ignoring an old savefile and starting a new" <+> title <+> "game."
      handleResponseUI cmd1
    (False, RespUpdAtomicUI UpdResume{}) -> do
      removeServerSave
      error $ T.unpack $
        "Savefile of client" <+> tshow side
        <+> "not usable. Removing server savefile. Please restart now."
    (False, RespUpdAtomicUI UpdRestart{}) -> do
      msgAdd $ "Welcome to" <+> title <> "!"
      -- Generate initial history. Only for UI clients.
      shistory <- defaultHistory $ configHistoryMax sconfig
      modifyClient $ \cli -> cli {shistory}
      handleResponseUI cmd1
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  fact <- getsState $ (EM.! side) . sfactionD
  when (isAIFact fact) $
    -- Prod the frontend to flush frames and start showing then continuously.
    void $ displayMore ColorFull "The team is under AI control (ESC to stop)."
  -- State and client state now valid.
  debugPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPrint $ "Frontend" <+> tshow side <+> "shutting down."
  liftIO $ fshutdown rf
  debugPrint $ "UI cliet" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponseUI cmd
    quit <- getsClient squit
    unless quit loop
