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
-- If the overlay is high long, it's truncated.
-- Similarly, for each line of the overlay, if it's too long, it's truncated.
drawOverlay :: MonadClientUI m
            => ColorMode -> Bool -> Overlay -> LevelId -> m SingleFrame
drawOverlay dm sfBlank topTrunc lid = do
  mbaseFrame <- if sfBlank
                then return Nothing
                else Just <$> drawBaseFrame dm lid
  return $! overlayFrame topTrunc mbaseFrame

promptGetKey :: MonadClientUI m
             => ColorMode -> Overlay -> Bool -> [K.KM] -> m K.KM
promptGetKey dm ov sfBlank frontKeyKeys = do
  lid <- viewedLevelUI
  keyPressed <- anyKeyPressed
  lastPlayOld <- getsSession slastPlay
  km <- case lastPlayOld of
    km : kms | not keyPressed && (null frontKeyKeys
                                  || km `elem` frontKeyKeys) -> do
      frontKeyFrame <- drawOverlay dm sfBlank ov lid
      displayFrame $ Just frontKeyFrame
      modifySession $ \sess -> sess {slastPlay = kms}
      Config{configRunStopMsgs} <- getsSession sconfig
      when configRunStopMsgs $ promptAdd $ "Voicing '" <> tshow km <> "'."
      return km
    _ : _ -> do
      -- We can't continue playback, so wipe out old slastPlay, srunning, etc.
      stopPlayBack
      discardPressedKey
      let ov2 = ov `glueOverlay` if keyPressed
                                 then [toAttrLine "*interrupted*"]
                                 else []
      frontKeyFrame <- drawOverlay dm sfBlank ov2 lid
      connFrontendFrontKey frontKeyKeys frontKeyFrame
    [] -> do
      frontKeyFrame <- drawOverlay dm sfBlank ov lid
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
animate :: MonadClientUI m => LevelId -> Animation -> m Frames
animate arena anim = do
  report <- getReportUI
  let truncRep = [renderReport report]
  basicFrame <- drawOverlay ColorFull False truncRep arena
  snoAnim <- getsClient $ snoAnim . sdebugCli
  return $! if fromMaybe False snoAnim
            then [Just basicFrame]
            else renderAnim basicFrame anim

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  let topRight = True
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid
  animMap <- rndToAction $ fadeout out topRight 2 lxsize lysize
  animFrs <- animate lid animMap
  mapM_ displayFrame $ tail animFrs  -- no basic frame between fadeout and in
  modifySession $ \sess -> sess {sdisplayNeeded = False}
