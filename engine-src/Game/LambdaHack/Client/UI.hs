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
  , ChanFrontend, chanFrontend, tryRestore, clientPrintUI
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
import           Game.LambdaHack.Common.ClientOptions
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
  modifySession $ \sess -> sess {slastLost = ES.empty}
  let loop :: Maybe ActorId -> m ReqUI
      loop mOldLeader = do
        keyPressed <- anyKeyPressed
        macroFrame <- getsSession smacroFrame
        -- This message, in particular, disturbs.
        when (keyPressed && not (null (unKeyMacro (keyPending macroFrame)))) $
          msgAdd MsgActionWarning "*interrupted*"
        report <- getsSession $ newReport . shistory
        modifySession $ \sess -> sess {sreportNull = nullVisibleReport report}
        slides <- reportToSlideshowKeepHalt []
        over <- case unsnoc slides of
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
              void $ getConfirms ColorFull [K.spaceKM, K.escKM] slides
              -- Indicate that report wiped out.
              modifySession $ \sess -> sess {sreportNull = True}
              -- Display base frame at the end.
              return []
        leader <- getLeaderUI
        b <- getsState $ getActorBody leader
        when (bhp b <= 0 && Just leader /= mOldLeader) $ displayMore ColorBW
          "If you move, the exertion will kill you. Consider asking for first aid instead."
        let ovs = EM.fromList [(propFont, over)]
        km <- promptGetKey ColorFull ovs False []
        abortOrCmd <- do
          -- Look up the key.
          CCUI{coinput=InputContent{bcmdMap}} <- getsSession sccui
          case km `M.lookup` bcmdMap of
            Just (_, _, cmd) -> do
              modifySession $ \sess -> sess {swaitTimes = if swaitTimes sess > 0
                                                          then - swaitTimes sess
                                                          else 0}
              restrictedCmdSemInCxtOfKM km cmd
            _ -> let msgKey = "unknown command '" <> K.showKM km <> "'"
                 in weaveJust <$> failWith (T.pack msgKey)
        -- GC macro stack if there are no actions left to handle,
        -- removing all unnecessary macro frames at once,
        -- but leaving the last one for user's in-game macros.
        modifySession $ \sess ->
          let (smacroFrameNew, smacroStackMew) =
                dropEmptyMacroFrames (smacroFrame sess) (smacroStack sess)
          in sess { smacroFrame = smacroFrameNew
                  , smacroStack = smacroStackMew }
        -- The command was failed or successful and if the latter,
        -- possibly took some time.
        case abortOrCmd of
          Right cmdS ->
            -- Exit the loop and let other actors act. No next key needed
            -- and no report could have been generated.
            return cmdS
          Left Nothing -> loop $ Just leader
          Left (Just err) -> do
            msgAdd MsgActionAlert $ showFailError err
            loop $ Just leader
  loop Nothing
