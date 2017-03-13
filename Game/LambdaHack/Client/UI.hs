-- | Ways for the client to use player input via UI to produce server
-- requests, based on the client's view (visualized for the player)
-- of the game state.
module Game.LambdaHack.Client.UI
  ( -- * Client UI monad
    MonadClientUI
    -- * Assorted UI operations
  , putSession, queryUI
  , displayRespUpdAtomicUI, displayRespSfxAtomicUI
    -- * Startup
  , KeyKind, SessionUI(..)
  , ChanFrontend, chanFrontend, frontendShutdown
    -- * Operations exposed for LoopClient
  , ColorMode(..)
  , reportToSlideshow, getConfirms, msgAdd, promptAdd, addPressedEsc
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
import qualified Data.Text as T

import qualified Game.LambdaHack.Client.UI.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.DisplayAtomicM
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.HandleHumanM
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.OverlayM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Client.UI.SlideshowM
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
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
    keyPressed <- anyKeyPressed
    if keyPressed then do
      discardPressedKey
      addPressedEsc
      -- Regaining control of faction cancels --stopAfter*.
      modifyClient $ \cli ->
        cli {sdebugCli = (sdebugCli cli) { sstopAfterSeconds = Nothing
                                         , sstopAfterFrames = Nothing }}
      if fleaderMode (gplayer fact) /= LeaderNull then
        return (ReqUIAutomate, Nothing)  -- stop AI
      else return (ReqUINop, Nothing)
    else do
      -- As long as UI faction is under AI control, check, once per move,
      -- for benchmark game stop.
      stopAfterFrames <- getsClient $ sstopAfterFrames . sdebugCli
      case stopAfterFrames of
        Nothing -> do
          stopAfterSeconds <- getsClient $ sstopAfterSeconds . sdebugCli
          case stopAfterSeconds of
            Nothing -> return (ReqUINop, Nothing)
            Just stopS -> do
              exit <- elapsedSessionTimeGT stopS
              if exit then do
                tellAllClipPS
                return (ReqUIGameExit, Nothing)  -- ask server to exit
              else return (ReqUINop, Nothing)
        Just stopF -> do
          allNframes <- getsSession sallNframes
          gnframes <- getsSession snframes
          if allNframes + gnframes >= stopF then do
            tellAllClipPS
            return (ReqUIGameExit, Nothing)  -- ask server to exit
          else return (ReqUINop, Nothing)
  else do
    let mleader = gleader fact
        !_A = assert (isJust mleader) ()
    req <- humanCommand
    leader2 <- getLeaderUI
    if mleader /= Just leader2
      then return (req, Just leader2)
      else return (req, Nothing)

-- | Let the human player issue commands until any command takes time.
humanCommand :: forall m. MonadClientUI m => m ReqUI
humanCommand = do
  modifySession $ \sess -> sess {slastLost = ES.empty}
  modifySession $ \sess -> sess {skeysHintMode = KeysHintAbsent}
  let loop :: m ReqUI
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
        slidesRaw <- reportToSlideshowKeep []
        over <- case unsnoc slidesRaw of
          Nothing -> return []
          Just (allButLast, (ov, _)) ->
            if allButLast == emptySlideshow
            then
              -- Display the only generated slide while waiting for next key.
              -- Strip the "--end-" prompt from it.
              return $! init ov
            else do
              -- Show, one by one, all slides, awaiting confirmation for each.
              void $ getConfirms ColorFull [K.spaceKM, K.escKM] slidesRaw
              -- Display base frame at the end.
              return []
        (seqCurrent, seqPrevious, k) <- getsSession slastRecord
        let slastRecord | k == 0 = ([], seqCurrent, 0)
                        | otherwise = ([], seqCurrent ++ seqPrevious, k - 1)
        modifySession $ \sess -> sess {slastRecord}
        lastPlay <- getsSession slastPlay
        leader <- getLeaderUI
        b <- getsState $ getActorBody leader
        when (bhp b <= 0) $ displayMore ColorBW
          "If you move, the exertion will kill you. Consider asking for first aid instead."
        km <- promptGetKey ColorFull over False []
        -- Messages shown, so update history and reset current report.
        when (null lastPlay) recordHistory
        abortOrCmd <- do
          -- Look up the key.
          Binding{bcmdMap} <- getsSession sbinding
          case km `M.lookup` bcmdMap of
            Just (_, _, cmd) -> do
              modifySession $ \sess -> sess
                {swaitTimes = if swaitTimes sess > 0
                              then - swaitTimes sess
                              else 0}
              cmdHumanSem cmd
            _ -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                 in weaveJust <$> failWith (T.pack msgKey)
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
