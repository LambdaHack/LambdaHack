{-# LANGUAGE CPP #-}
-- | Ways for the client to use player input via UI to produce server
-- requests, based on the client's view (visualized for the player)
-- of the game state.
module Game.LambdaHack.Client.UI
  ( -- * Client UI monad
    MonadClientUI
    -- * Assorted UI operations
  , queryUI, pongUI
  , displayRespUpdAtomicUI, displayRespSfxAtomicUI
    -- * Startup
  , srtFrontend, KeyKind, SessionUI
    -- * Operations exposed for LoopClient
  , ColorMode(..), displayMore, msgAdd
#ifdef EXPOSE_INTERNAL
  , humanCommand
#endif
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Maybe

import Game.LambdaHack.Atomic
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.DisplayAtomicClient
import Game.LambdaHack.Client.UI.HandleHumanClient
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.StartupFrontendClient
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

-- | Handle the move of a UI player.
queryUI :: MonadClientUI m => m RequestUI
queryUI = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let (leader, mtgt) = fromMaybe (assert `failure` fact) $ gleader fact
  req <- humanCommand
  leader2 <- getLeaderUI
  mtgt2 <- getsClient $ fmap fst . EM.lookup leader2 . stargetD
  if (leader2, mtgt2) /= (leader, mtgt)
    then return $! ReqUILeader leader2 mtgt2 req
    else return $! req

-- | Let the human player issue commands, until any command takes time.
humanCommand :: forall m. MonadClientUI m => m RequestUI
humanCommand = do
  -- For human UI we invalidate whole @sbfsD@ at the start of each
  -- UI player input, which is an overkill, but doesn't affects
  -- screensavers, because they are UI, but not human.
  modifyClient $ \cli -> cli {sbfsD = EM.empty, slastLost = ES.empty}
  let loop :: Maybe (Bool, Overlay) -> m RequestUI
      loop mover = do
        (lastBlank, over) <- case mover of
          Nothing -> do
            -- Display current state and keys if no slideshow or if interrupted.
            keys <- describeMainKeys
            sli <- promptToSlideshow keys
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
        lastPlay <- getsClient slastPlay
        km <- getKeyOverlayCommand lastBlank over
        -- Messages shown, so update history and reset current report.
        when (null lastPlay) recordHistory
        abortOrCmd <- do
          -- Look up the key.
          Binding{bcmdMap} <- askBinding
          case M.lookup km{K.pointer=dummyPoint} bcmdMap of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              modifyClient $ \cli -> cli
                {swaitTimes = if swaitTimes cli > 0
                              then - swaitTimes cli
                              else 0}
              escAI <- getsClient sescAI
              case escAI of
                EscAIStarted -> do
                  modifyClient $ \cli -> cli {sescAI = EscAIMenu}
                  cmdHumanSem cmd
                EscAIMenu -> do
                  unless (km `elem` [K.escKM, K.returnKM]) $
                    modifyClient $ \cli -> cli {sescAI = EscAIExited}
                  cmdHumanSem cmd
                _ -> do
                  modifyClient $ \cli -> cli {sescAI = EscAINothing}
                  stgtMode <- getsClient stgtMode
                  if km == K.escKM && isNothing stgtMode && isJust mover
                  then cmdHumanSem Clear
                  else cmdHumanSem cmd
            Nothing -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                       in failWith msgKey
        -- The command was failed or successful and if the latter,
        -- possibly took some time.
        case abortOrCmd of
          Right cmdS ->
            -- Exit the loop and let other actors act. No next key needed
            -- and no slides could have been generated.
            return cmdS
          Left slides -> do
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained slides.
            let (onBlank, sli) = slideshow slides
            mLast <- case sli of
              [] -> return Nothing
              [sLast] ->
                -- Avoid displaying the single slide twice.
                return $ Just (onBlank, sLast)
              _ -> do
                -- Show, one by one, all slides, awaiting confirmation
                -- for all but the last one (which is displayed twice, BTW).
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                go <- getInitConfirms ColorFull [km] slides
                return $! if go then Just (onBlank, last sli) else Nothing
            loop mLast
  loop Nothing

-- | Client signals to the server that it's still online, flushes frames
-- (if needed) and sends some extra info.
pongUI :: MonadClientUI m => m RequestUI
pongUI = do
  escPressed <- tryTakeMVarSescMVar
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let pong ats = return $ ReqUIPong ats
      underAI = isAIFact fact
  if escPressed && underAI && fleaderMode (gplayer fact) /= LeaderNull then do
    modifyClient $ \cli -> cli {sescAI = EscAIStarted}
    -- Ask server to turn off AI for the faction's leader.
    let atomicCmd = UpdAtomic $ UpdAutoFaction side False
    pong [atomicCmd]
  else do
    -- Respond to the server normally, perhaps pinging the frontend, too.
    when underAI syncFrames
    pong []
