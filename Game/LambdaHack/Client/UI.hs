{-# LANGUAGE CPP #-}
-- | Ways for the client to use player input via UI to produce server
-- requests, based on the client's view (visualized for the player)
-- of the game state.
module Game.LambdaHack.Client.UI
  ( -- * Client UI monad
    MonadClientUI(putSession)
    -- * Assorted UI operations
  , queryUI
  , displayRespUpdAtomicUI, displayRespSfxAtomicUI
    -- * Startup
  , KeyKind, SessionUI(..)
  , ChanFrontend, chanFrontend, frontendShutdown
    -- * Operations exposed for LoopClient
  , ColorMode(..), displayMore, msgAdd
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , humanCommand
#endif
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Maybe

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.DisplayAtomicClient
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.HandleHumanClient
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

-- | Handle the move of a UI player.
queryUI :: MonadClientUI m => m RequestUI
queryUI = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  if isAIFact fact then do
    -- TODO: allow any action that does not take time, e.g., changing
    -- leaders, levels, moving xhair. Only ESC then stops AI.
    keyPressed <- anyKeyPressed
    if keyPressed then do
      discardPressedKey
      if fleaderMode (gplayer fact) /= LeaderNull then
        return ReqUIAutomate  -- stop AI
      else return ReqUINop  -- somehow stop? restart?
    else return ReqUINop
  else do
    let (leader, mtgt) = fromMaybe (assert `failure` fact) $ gleader fact
    req <- humanCommand
    leader2 <- getLeaderUI
    mtgt2 <- getsClient $ fmap fst . EM.lookup leader2 . stargetD
    if (leader2, mtgt2) /= (leader, mtgt)
      then return $! ReqUILeader leader2 mtgt2 req
      else return $! req

-- | Let the human player issue commands until any command takes time.
humanCommand :: forall m. MonadClientUI m => m RequestUI
humanCommand = do
  -- For human UI we invalidate whole @sbfsD@ at the start of each
  -- UI player input that start a player move, which is an overkill,
  -- but doesn't slow screensavers, because they are UI,
  -- but not human.
  modifyClient $ \cli -> cli {sbfsD = EM.empty}
  modifySession $ \sess -> sess {slastLost = ES.empty}
  let loop :: Either Bool Overlay -> m RequestUI
      loop mover = do
        over <- case mover of
          Left b -> do
            -- Display current state and keys if no slideshow or if interrupted.
            keys <- if b then describeMainKeys else return ""
            sli <- promptToSlideshow keys
            return $! head $ slideshow sli  -- only the first slide of keys; OK
          Right bLast ->
            -- Display the last generated slide while waiting for the next key.
            return bLast
        (seqCurrent, seqPrevious, k) <- getsSession slastRecord
        case k of
          0 -> do
            let slastRecord = ([], seqCurrent, 0)
            modifySession $ \sess -> sess {slastRecord}
          _ -> do
            let slastRecord = ([], seqCurrent ++ seqPrevious, k - 1)
            modifySession $ \sess -> sess {slastRecord}
        lastPlay <- getsSession slastPlay
        km <- promptGetKey over False []
        -- Messages shown, so update history and reset current report.
        when (null lastPlay) recordHistory
        abortOrCmd <- do
          -- Look up the key.
          Binding{bcmdMap} <- askBinding
          case km `M.lookup` bcmdMap of
            Just (cats, _, cmd) | CmdMainMenu `notElem` cats
                                  && CmdSettingsMenu `notElem` cats -> do
              -- Query and clear the last command key.
              modifySession $ \sess -> sess
                {swaitTimes = if swaitTimes sess > 0
                              then - swaitTimes sess
                              else 0}
              cmdHumanSem cmd
            _ -> let msgKey = "unknown command <" <> K.showKM km <> ">"
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
            let sli = slideshow slides
            mLast <- case sli of
              [] -> do
                saimMode <- getsSession saimMode
                return $ Left $ isJust saimMode
              [sLast] ->
                -- Avoid displaying the single slide twice.
                return $ Right sLast
              _ -> do
                -- Show, one by one, all slides, awaiting confirmation for each.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                go <- getConfirms ColorFull [K.spaceKM] [K.escKM] slides
                return $ Left $ not go
            loop mLast
  loop $ Left False
