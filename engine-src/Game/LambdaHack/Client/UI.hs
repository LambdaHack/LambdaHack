-- | Ways for the client to use player input via UI to produce server
-- requests, based on the client's view (visualized for the player)
-- of the game state.
module Game.LambdaHack.Client.UI
  ( -- * Querying the human player
    queryUI
    -- * UI monad and session type
  , MonadClientUI(..), SessionUI(..)
    -- * Updating UI state wrt game state changes
  , displayRespUpdAtomicUI, displayRespSfxAtomicUI
    -- * Startup and initialization
  , CCUI(..)
  , UIOptions, applyUIOptions, uCmdline, mkUIOptions
    -- * Operations exposed for "Game.LambdaHack.Client.LoopM"
  , ChanFrontend, chanFrontend, promptAdd, tryRestore
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , humanCommand
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.Request
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.DisplayAtomicM
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.Frontend
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Client.UI.UIOptionsParse
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.ModeKind

-- | Handle the move of a human player.
queryUI :: (MonadClient m, MonadClientUI m) => m RequestUI
queryUI = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  if isAIFact fact then do
    recordHistory
    keyPressed <- anyKeyPressed
    if keyPressed && fleaderMode (gplayer fact) /= LeaderNull then do
      -- Menu is entered in @displayRespUpdAtomicUI@ at @UpdAutoFaction@.
      discardPressedKey
      -- Regaining control of faction cancels some --stopAfter*.
      modifyClient $ \cli ->
        cli {soptions = (soptions cli) { sstopAfterSeconds = Nothing
                                       , sstopAfterFrames = Nothing }}
      return (ReqUIAutomate, Nothing)  -- stop AI
    else do
      -- As long as UI faction is under AI control, check, once per move,
      -- for benchmark game stop.
      stopAfterFrames <- getsClient $ sstopAfterFrames . soptions
      bench <- getsClient $ sbenchmark . soptions
      let exitCmd = if bench then ReqUIGameDropAndExit else ReqUIGameSaveAndExit
      case stopAfterFrames of
        Nothing -> do
          stopAfterSeconds <- getsClient $ sstopAfterSeconds . soptions
          case stopAfterSeconds of
            Nothing -> return (ReqUINop, Nothing)
            Just stopS -> do
              exit <- elapsedSessionTimeGT stopS
              if exit then do
                tellAllClipPS
                return (exitCmd, Nothing)  -- ask server to exit
              else return (ReqUINop, Nothing)
        Just stopF -> do
          allNframes <- getsSession sallNframes
          gnframes <- getsSession snframes
          if allNframes + gnframes >= stopF then do
            tellAllClipPS
            return (exitCmd, Nothing)  -- ask server to exit
          else return (ReqUINop, Nothing)
  else do
    let mleader = gleader fact
        !_A = assert (isJust mleader) ()
    req <- humanCommand
    leader2 <- getLeaderUI
    -- Don't send the leader switch to the server with these commands,
    -- to avoid leader death at resume if his HP <= 0. That would violate
    -- the principle that save and reload doesn't change game state.
    let saveCmd cmd = case cmd of
          ReqUIGameDropAndExit -> True
          ReqUIGameSaveAndExit -> True
          ReqUIGameSave -> True
          _ -> False
    return (req, if mleader /= Just leader2 && not (saveCmd req)
                 then Just leader2
                 else Nothing)

-- | Let the human player issue commands until any command takes time.
humanCommand :: forall m. (MonadClient m, MonadClientUI m) => m ReqUI
humanCommand = do
  FontSetup{propFont} <- getFontSetup
  modifySession $ \sess -> sess { slastLost = ES.empty
                                , shintMode = HintAbsent }
  let loop :: Maybe ActorId -> m ReqUI
      loop mOldLeader = do
        report <- getsSession $ newReport . shistory
        hintMode <- getsSession shintMode
        -- Hints are not considered non-empty reports.
        modifySession $ \sess -> sess
          {sreportNull = nullReport report || hintMode == HintShown}
        case hintMode of
          HintAbsent -> return ()
          HintShown -> modifySession $ \sess -> sess {shintMode = HintWiped}
          HintWiped -> modifySession $ \sess -> sess {shintMode = HintAbsent}
        slidesRaw <- reportToSlideshowKeep []
        over <- case unsnoc slidesRaw of
          Nothing -> return []
          Just (allButLast, (ov, _)) ->
            if allButLast == emptySlideshow
            then do
              -- Display the only generated slide while waiting for next key.
              -- Strip the "--end-" prompt from it, by ignoring @MonoFont@.
              let ovProp = ov EM.! propFont
              return $! if EM.size ov > 1 then ovProp else init ovProp
            else do
              -- Show, one by one, all slides, awaiting confirmation for each.
              void $ getConfirms ColorFull [K.spaceKM, K.escKM] slidesRaw
              -- Indicate that report wiped out.
              modifySession $ \sess -> sess {sreportNull = True}
              -- Display base frame at the end.
              return []
        leader <- getLeaderUI
        b <- getsState $ getActorBody leader
        when (bhp b <= 0 && Just leader /= mOldLeader) $ displayMore ColorBW
          "If you move, the exertion will kill you. Consider asking for first aid instead."
        km <- promptGetKey ColorFull (EM.fromList [(propFont, over)]) False []
        abortOrCmd <- do
          -- Look up the key.
          CCUI{coinput=InputContent{bcmdMap}} <- getsSession sccui
          case km `M.lookup` bcmdMap of
            Just (_, _, cmd) -> do
              -- Repeating last action key does not cover menu navigation
              -- keypresses, so here's the right place to record it.
              modifySession $ \sess ->
                sess { swaitTimes =
                         if swaitTimes sess > 0
                         then - swaitTimes sess
                         else 0
                     , sactionPending = case cmd of
                         (HumanCmd.RepeatLast _) -> sactionPending sess
                         -- Don't save repeating last action key as the last
                         -- key handled.
                         _ ->
                           let oldBuffer = head (sactionPending sess)
                               newBuffer = oldBuffer { slastAction = Just km }
                            in newBuffer : tail (sactionPending sess)
                     }
              cmdHumanSem cmd
            _ -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                 in weaveJust <$> failWith (T.pack msgKey)
        -- The command was failed or successful and if the latter,
        -- possibly took some time.
        modifySession $ \sess ->
          sess { sactionPending = case sactionPending sess of
                   ActionBuffer _ (KeyMacro kms) _ : as
                     | not (null as) && null kms -> as
                   _ -> sactionPending sess }
            -- GC action buffer if there's no actions left to handle;
            -- leave the last one as a buffer for user's in-game macros.
        case abortOrCmd of
          Right cmdS ->
            -- Exit the loop and let other actors act. No next key needed
            -- and no report could have been generated.
            return cmdS
          Left Nothing -> loop $ Just leader
          Left (Just err) -> do
            -- Avoid "*never mind*<x4>".
            let l0 = ["*never mind*", "*aiming started*"]
                t = showFailError err
            if t `elem` l0 then msgAdd0 MsgAlert t
                           else msgAdd MsgAlert t
            loop $ Just leader
  loop Nothing
