-- | Semantics of most 'ResponseAI' client commands.
module Game.LambdaHack.Client.UI
  ( -- * Client UI monad
    MonadClientUI
  , queryUI
  , Config(..), mkConfig
  , displayRespUpdAtomicUI, displayRespSfxAtomicUI
    -- TODO: get rid of those:
  , SessionUI(..), connFrontend
  , syncFrames, tryTakeMVarSescMVar, displayMore, msgAdd, stdBinding
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import Data.Maybe

import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.DisplayCmdAtomicClient
import Game.LambdaHack.Client.UI.HandleHumanCmdClient
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.RunClient
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State

-- | Handle the move of a UI player.
queryUI :: MonadClientUI m => ActorId -> m Request
queryUI aid = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  -- When running, stop if disturbed. If not running, let the human
  -- player issue commands, until any command takes time.
  leader <- getLeaderUI
  assert (leader == aid `blame` "player moves not his leader"
                        `twith` (leader, aid)) skip
  srunning <- getsClient srunning
  case srunning of
    Nothing -> humanCommand Nothing
    Just RunParams{runMembers} | isSpawnFact fact && runMembers /= [aid] -> do
      stopRunning
      Config{configRunStopMsgs} <- getConfig
      let msg = if configRunStopMsgs
                then Just $ "Run stop: spawner leader change"
                else Nothing
      humanCommand msg
    Just runParams -> do
      runOutcome <- continueRun runParams
      case runOutcome of
        Left stopMsg -> do
          stopRunning
          Config{configRunStopMsgs} <- getConfig
          let msg = if configRunStopMsgs
                    then Just $ "Run stop:" <+> stopMsg
                    else Nothing
          humanCommand msg
        Right (paramNew, runCmd) -> do
          modifyClient $ \cli -> cli {srunning = Just paramNew}
          return $ ReqTimed runCmd

-- | Determine and process the next human player command. The argument is
-- the last stop message due to running, if any.
humanCommand :: forall m. MonadClientUI m
             => Maybe Msg
             -> m Request
humanCommand msgRunStop = do
  -- For human UI we invalidate whole @sbfsD@ at the start of each
  -- UI player input, which is an overkill, but doesn't affects
  -- screensavers, because they are UI, but not human.
  modifyClient $ \cli -> cli {sbfsD = EM.empty}
  let loop :: Maybe (Bool, Overlay) -> m Request
      loop mover = do
        (lastBlank, over) <- case mover of
          Nothing -> do
            -- Display current state if no slideshow or if interrupted.
            modifyClient $ \cli -> cli {slastKey = Nothing}
            sli <- promptToSlideshow ""
            return (False, head . snd $! slideshow sli)
          Just bLast ->
            -- (Re-)display the last slide while waiting for the next key.
            return bLast
        (seqCurrent, seqPrevious, k) <- getsClient slastRecord
        case k of
          0 -> do
            let slastRecord = ([], seqCurrent, 0)
            modifyClient $ \cli -> cli {slastRecord}
          _ -> do
            let slastRecord = ([], seqCurrent ++ seqPrevious, k - 1)
            modifyClient $ \cli -> cli {slastRecord}
        km <- getKeyOverlayCommand lastBlank over
        -- Messages shown, so update history and reset current report.
        recordHistory
        abortOrCmd <- do
          -- Look up the key.
          Binding{bcmdMap} <- askBinding
          case M.lookup km bcmdMap of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              lastKey <- getsClient slastKey
              stgtMode <- getsClient stgtMode
              modifyClient $ \cli -> cli
                {swaitTimes = if swaitTimes cli > 0
                              then - swaitTimes cli
                              else 0}
              if Just km == lastKey
                 || km == K.escKey && isNothing stgtMode && isJust mover
                then do
                  modifyClient $ \cli -> cli {slastKey = Nothing}
                  cmdHumanSem Clear
                else do
                  modifyClient $ \cli -> cli {slastKey = Just km}
                  cmdHumanSem cmd
            Nothing -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                       in fmap Left $ promptToSlideshow msgKey
        -- The command was failed or successful and if the latter,
        -- possibly took some time.
        case abortOrCmd of
          Right cmdS -> do
            -- Exit the loop and let other actors act. No next key needed
            -- and no slides could have been generated.
            modifyClient $ \cli -> cli {slastKey = Nothing}
            case cmdS of
              ReqTimed cmd ->
                modifyClient $ \cli -> cli {slastCmd = Just cmd}
              _ -> return ()
            return cmdS
          Left slides -> do
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained slides.
            let (onBlank, sli) = slideshow slides
            mLast <- case reverse sli  of
              [] -> return Nothing
              [sLast] -> return $ Just (onBlank, sLast)
              sls@(sLast : _) -> do
                -- Show, one by one, all slides, awaiting confirmation
                -- for all but the last one.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                go <- getInitConfirms ColorFull [km]
                      $ toSlideshow onBlank $ reverse $ map overlay sls
                return $! if go then Just (onBlank, sLast) else Nothing
            loop mLast
  case msgRunStop of
    Nothing -> loop Nothing
    Just msg -> do
      sli <- promptToSlideshow msg
      loop $ Just (False, head . snd $ slideshow sli)
