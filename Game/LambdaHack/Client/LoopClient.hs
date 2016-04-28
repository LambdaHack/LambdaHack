{-# LANGUAGE FlexibleContexts #-}
-- | The main loop of the client, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Client.LoopClient (loopAI, loopUI) where

import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import System.FilePath

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.FileClient
import Game.LambdaHack.Client.HandleResponseClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.ProtocolClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind

restoreGame :: (Binary sess, MonadClient m)
            => m (Maybe (State, StateClient, sess))
restoreGame = do
  bench <- getsClient $ sbenchmark . sdebugCli
  if bench then return Nothing
  else do
    Kind.COps{corule} <- getsState scops
    let stdRuleset = Kind.stdRuleset corule
        pathsDataFile = rpathsDataFile stdRuleset
        cfgUIName = rcfgUIName stdRuleset
    side <- getsClient sside
    isAI <- getsClient sisAI
    prefix <- getsClient $ ssavePrefixCli . sdebugCli
    let copies = [( "GameDefinition" </> cfgUIName <.> "default"
                  , cfgUIName <.> "ini" )]
        name = prefix <.> saveName side isAI
    liftIO $ Save.restoreGame tryCreateDir tryCopyDataFiles strictDecodeEOF
                              name copies pathsDataFile

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
  restoredG <- restoreGame
  restored <- case restoredG of
    Just (s, cli, ()) | not $ snewGameCli sdebugCli -> do  -- Restore game.
      let sCops = updateCOps (const cops) s
      handleResponseAI $ RespUpdAtomicAI $ UpdResumeServer sCops
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
  schanF <- liftIO $ chanFrontend sdebugCli
  let !sbinding = stdBinding copsClient sconfig  -- evaluate to check for errors
  -- Warning: state and client state are invalid here, e.g., sdungeon
  -- and sper are empty.
  cops <- getsState scops
  modifyClient $ \cli -> cli {sdebugCli}
  restoredG <- restoreGame
  restored <- case restoredG of
    Just (s, cli, sess) | not $ snewGameCli sdebugCli -> do  -- Restore game.
      let sCops = updateCOps (const cops) s
      handleResponseUI $ RespUpdAtomicUI $ UpdResumeServer sCops
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
  Kind.COps{corule} <- getsState scops
  let title = rtitle $ Kind.stdRuleset corule
  side <- getsClient sside
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
      modifySession $ \sess -> sess {shistory}
      handleResponseUI cmd1
    _ -> assert `failure` "unexpected command" `twith` (side, restored, cmd1)
  fact <- getsState $ (EM.! side) . sfactionD
  when (isAIFact fact) $
    -- Prod the frontend to flush frames and start showing then continuously.
    void $ displayMore ColorFull "The team is under AI control (press any key to stop)."
  -- State and client state now valid.
  debugPrint $ "UI client" <+> tshow side <+> "started."
  loop
  debugPrint $ "Frontend" <+> tshow side <+> "shutting down."
  frontendShutdown
  debugPrint $ "UI cliet" <+> tshow side <+> "stopped."
 where
  loop = do
    cmd <- receiveResponse
    handleResponseUI cmd
    quit <- getsClient squit
    unless quit loop
