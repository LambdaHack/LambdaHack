-- | A set of Frame monad operations.
module Game.LambdaHack.Client.UI.FrameM
  ( pushFrame, promptGetKey, stopPlayBack, animate, fadeOutOrIn
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , drawOverlay, renderFrames
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.Animation
import           Game.LambdaHack.Client.UI.DrawM
import           Game.LambdaHack.Client.UI.Frame
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State

-- | Draw the current level with the overlay on top.
-- If the overlay is too long, it's truncated.
-- Similarly, for each line of the overlay, if it's too wide, it's truncated.
drawOverlay :: MonadClientUI m
            => ColorMode -> Bool -> Overlay -> LevelId -> m FrameForall
drawOverlay dm onBlank topTrunc lid = do
  mbaseFrame <- if onBlank
                then return $ FrameForall $ \_v -> return ()
                else drawBaseFrame dm lid
  return $! overlayFrameWithLines onBlank topTrunc mbaseFrame

-- | Push the frame depicting the current level to the frame queue.
-- Only one line of the report is shown, as in animations,
-- because it may not be our turn, so we can't clear the message
-- to see what is underneath.
pushFrame :: MonadClientUI m => m ()
pushFrame = do
  -- The delay before reaction to keypress was too long in case of many
  -- projectiles flying and ending flight, so frames need to be skipped.
  keyPressed <- anyKeyPressed
  unless keyPressed $ do
    lidV <- viewedLevelUI
    report <- getReportUI
    let truncRep = [renderReport report]
    frame <- drawOverlay ColorFull False truncRep lidV
    displayFrames lidV [Just frame]

promptGetKey :: MonadClientUI m
             => ColorMode -> Overlay -> Bool -> [K.KM] -> m K.KM
promptGetKey dm ov onBlank frontKeyKeys = do
  lidV <- viewedLevelUI
  keyPressed <- anyKeyPressed
  lastPlayOld <- getsSession slastPlay
  km <- case lastPlayOld of
    km : kms | not keyPressed && (null frontKeyKeys
                                  || km `elem` frontKeyKeys) -> do
      frontKeyFrame <- drawOverlay dm onBlank ov lidV
      displayFrames lidV [Just frontKeyFrame]
      modifySession $ \sess -> sess {slastPlay = kms}
      UIOptions{uRunStopMsgs} <- getsSession sUIOptions
      when uRunStopMsgs $ promptAdd0 $ "Voicing '" <> tshow km <> "'."
      return km
    _ : _ -> do
      -- We can't continue playback, so wipe out old slastPlay, srunning, etc.
      stopPlayBack
      discardPressedKey
      let ov2 = ov `glueLines` [stringToAL "*interrupted*" | keyPressed]
      frontKeyFrame <- drawOverlay dm onBlank ov2 lidV
      connFrontendFrontKey frontKeyKeys frontKeyFrame
    [] -> do
      -- If we ask for a key, then we don't want to run any more
      -- and we want to avoid changing leader back to initial run leader
      -- at the nearest @stopPlayBack@, etc.
      modifySession $ \sess -> sess {srunning = Nothing}
      frontKeyFrame <- drawOverlay dm onBlank ov lidV
      connFrontendFrontKey frontKeyKeys frontKeyFrame
  LastRecord seqCurrent seqPrevious k <- getsSession slastRecord
  let slastRecord = LastRecord (km : seqCurrent) seqPrevious k
  modifySession $ \sess -> sess { slastRecord
                                , sdisplayNeeded = False }
  return km

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = do
  modifySession $ \sess -> sess
    { slastPlay = []
    , slastRecord = LastRecord [] [] 0
        -- Needed to cancel macros that contain apostrophes.
    }
  srunning <- getsSession srunning
  case srunning of
    Nothing -> return ()
    Just RunParams{runLeader} -> do
      -- Switch to the original leader, from before the run start,
      -- unless dead or unless the faction never runs with multiple
      -- (but could have the leader changed automatically meanwhile).
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      arena <- getArenaUI
      s <- getState
      when (memActor runLeader arena s && not (noRunWithMulti fact)) $
        modifyClient $ updateLeader runLeader s
      modifySession (\sess -> sess {srunning = Nothing})

-- | Render animations on top of the current screen frame.
renderFrames :: MonadClientUI m => LevelId -> Animation -> m Frames
renderFrames arena anim = do
  report <- getReportUI
  let truncRep = [renderReport report]
  basicFrame <- drawOverlay ColorFull False truncRep arena
  snoAnim <- getsClient $ snoAnim . soptions
  return $! if fromMaybe False snoAnim
            then [Just basicFrame]
            else renderAnim basicFrame anim

-- | Render and display animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m ()
animate arena anim = do
  -- The delay before reaction to keypress was too long in case of many
  -- projectiles hitting actors, so frames need to be skipped.
  keyPressed <- anyKeyPressed
  unless keyPressed $ do
    frames <- renderFrames arena anim
    displayFrames arena frames

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  arena <- getArenaUI
  Level{lxsize, lysize} <- getLevel arena
  animMap <- rndToActionForget $ fadeout out 2 lxsize lysize
  animFrs <- renderFrames arena animMap
  displayFrames arena (tail animFrs)  -- no basic frame between fadeout and in
