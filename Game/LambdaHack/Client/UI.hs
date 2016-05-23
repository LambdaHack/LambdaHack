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
  , ColorMode(..), displayMore, msgAdd, promptAdd
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , humanCommand
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.DisplayAtomicM
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.HandleHumanM
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.OverlayM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Client.UI.SlideshowM
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
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
    -- leaders, levels, moving xhair. Only ESC and mouse then stops AI.
    keyPressed <- anyKeyPressed
    if keyPressed then do
      discardPressedKey
      addPressedKey KMP {kmpKeyMod = K.escKM, kmpPointer = originPoint}
      if fleaderMode (gplayer fact) /= LeaderNull then
        return ReqUIAutomate  -- stop AI
      else return ReqUINop  -- TODO: somehow stop? restart?
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
  modifySession $ \sess -> sess {skeysHintMode = KeysHintAbsent}
  let loop :: m RequestUI
      loop = do
        report <- getsSession _sreport
        if nullReport report then do
          -- Display keys sometimes, alternating with empty screen.
          keysHintMode <- getsSession skeysHintMode
          case keysHintMode of
            KeysHintPresent -> describeMainKeys >>= promptAdd
            KeysHintBlocked ->
              modifySession $ \sess -> sess {skeysHintMode = KeysHintAbsent}
            _ -> return ()
        else modifySession $ \sess -> sess {skeysHintMode = KeysHintBlocked}
        slidesRaw <- reportToSlideshow []
        over <- case unsnoc slidesRaw of
          Nothing -> return []
          Just (allButLast, (ov, _)) -> do
            -- Show, one by one, all but last, awaiting confirmation for each.
            void $ getConfirms ColorFull [K.spaceKM, K.escKM] allButLast
            -- Display the last generated slide while waiting for next key.
            -- Strip the "--end-" prompt from it.
            return $ init ov
        (seqCurrent, seqPrevious, k) <- getsSession slastRecord
        let slastRecord | k == 0 = ([], seqCurrent, 0)
                        | otherwise = ([], seqCurrent ++ seqPrevious, k - 1)
        modifySession $ \sess -> sess {slastRecord}
        lastPlay <- getsSession slastPlay
        km <- promptGetKey ColorFull over False []
        -- Messages shown, so update history and reset current report.
        when (null lastPlay) recordHistory
        abortOrCmd <- do
          -- Look up the key.
          Binding{bcmdMap} <- getsSession sbinding
          case km `M.lookup` bcmdMap of
            Just (cats, _, cmd) | CmdMainMenu `notElem` cats
                                  && CmdSettingsMenu `notElem` cats -> do
              modifySession $ \sess -> sess
                {swaitTimes = if swaitTimes sess > 0
                              then - swaitTimes sess
                              else 0}
              cmdHumanSem cmd
            _ -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                 in weaveJust <$> failWith msgKey
        -- The command was failed or successful and if the latter,
        -- possibly took some time.
        case abortOrCmd of
          Right cmdS ->
            -- Exit the loop and let other actors act. No next key needed
            -- and no report could have been generated.
            return cmdS
          Left Nothing -> loop
          Left (Just err) -> do
            stopPlayBack
            promptAdd $ showFailError err
            loop
  loop
