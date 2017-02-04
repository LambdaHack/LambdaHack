-- | A set of Frame monad operations.
module Game.LambdaHack.Client.UI.FrameM
  ( drawOverlay, promptGetKey, stopPlayBack, animate, fadeOutOrIn
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.DrawM
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State

-- | Draw the current level with the overlay on top.
-- If the overlay is too long, it's truncated.
-- Similarly, for each line of the overlay, if it's too wide, it's truncated.
drawOverlay :: MonadClientUI m
            => ColorMode -> Bool -> [AttrLine] -> LevelId -> m FrameForall
drawOverlay dm onBlank topTrunc lid = do
  mbaseFrame <- if onBlank
                then return $ FrameForall $ \_v -> return ()
                else drawBaseFrame dm lid
  return $! overlayFrameWithLines onBlank topTrunc mbaseFrame

promptGetKey :: MonadClientUI m
             => ColorMode -> [AttrLine] -> Bool -> [K.KM] -> m K.KM
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
      Config{configRunStopMsgs} <- getsSession sconfig
      when configRunStopMsgs $ promptAdd $ "Voicing '" <> tshow km <> "'."
      return km
    _ : _ -> do
      -- We can't continue playback, so wipe out old slastPlay, srunning, etc.
      stopPlayBack
      discardPressedKey
      let ov2 = ov `glueLines` if keyPressed
                                 then [stringToAL "*interrupted*"]
                                 else []
      frontKeyFrame <- drawOverlay dm onBlank ov2 lidV
      connFrontendFrontKey frontKeyKeys frontKeyFrame
    [] -> do
      frontKeyFrame <- drawOverlay dm onBlank ov lidV
      connFrontendFrontKey frontKeyKeys frontKeyFrame
  (seqCurrent, seqPrevious, k) <- getsSession slastRecord
  let slastRecord = (km : seqCurrent, seqPrevious, k)
  modifySession $ \sess -> sess { slastRecord
                                , sdisplayNeeded = False }
  return km

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = do
  modifySession $ \sess -> sess
    { slastPlay = []
    , slastRecord = ([], [], 0)
        -- Needed to cancel macros that contain apostrophes.
    , swaitTimes = - abs (swaitTimes sess)
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

-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
renderFrames :: MonadClientUI m => LevelId -> Animation -> m Frames
renderFrames arena anim = do
  report <- getReportUI
  let truncRep = [renderReport report]
  basicFrame <- drawOverlay ColorFull False truncRep arena
  snoAnim <- getsClient $ snoAnim . sdebugCli
  return $! if fromMaybe False snoAnim
            then [Just basicFrame]
            else renderAnim basicFrame anim

-- | Render and display animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m ()
animate arena anim = do
  frames <- renderFrames arena anim
  displayFrames arena frames

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  arena <- getArenaUI
  Level{lxsize, lysize} <- getLevel arena
  animMap <- rndToActionForget $ fadeout out 2 lxsize lysize
  animFrs <- renderFrames arena animMap
  displayFrames arena (tail animFrs)  -- no basic frame between fadeout and in
