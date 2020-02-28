-- | A set of Frame monad operations.
module Game.LambdaHack.Client.UI.FrameM
  ( pushFrame, promptGetKey, addToMacro, stopPlayBack, animate, fadeOutOrIn
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , drawOverlay, renderFrames, resetPlayBack
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.Animation
import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.DrawM
import           Game.LambdaHack.Client.UI.Frame
import qualified Game.LambdaHack.Client.UI.Frontend as Frontend
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Definition.Color as Color

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m
            => ColorMode -> Bool -> FontOverlayMap -> LevelId
            -> m PreFrame3
drawOverlay dm onBlank ovs lid = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  basicFrame <- if onBlank
                then do
                  let m = U.replicate (rwidth * rheight)
                                      (Color.attrCharW32 Color.spaceAttrW32)
                  return (m, FrameForall $ \_v -> return ())
                else drawHudFrame dm lid
  FontSetup{..} <- getFontSetup
  soptions <- getsClient soptions
  let isTeletype = Frontend.frontendName soptions == "teletype"
      ovProp = if multiFont
               then truncateOverlay False (4 * rwidth) rheight False 0 onBlank
                    $ EM.findWithDefault [] propFont ovs
               else if isTeletype  -- hack for debug output
                    then concat $ EM.elems ovs
                    else []
      ovMono = if multiFont
               then truncateOverlay False (2 * rwidth) rheight False 0 onBlank
                    $ EM.findWithDefault [] monoFont ovs
               else []
      ovOther = if multiFont
                then truncateOverlay True rwidth rheight False 0 onBlank
                     $ EM.findWithDefault [] squareFont ovs
                else if isTeletype  -- hack for debug output
                     then []
                     else truncateOverlay True rwidth rheight True 20 onBlank
                          $ concat $ EM.elems ovs
                            -- 20 needed not to leave gaps in skills menu
                            -- in the absence of backdrop
      ovBackdrop =
        if multiFont
        then let propOutline =
                   truncateOverlay False (4 * rwidth) rheight True 0 onBlank
                   $ EM.findWithDefault [] propFont ovs
                 monoOutline =
                   truncateOverlay False (2 * rwidth) rheight True 0 onBlank
                    $ EM.findWithDefault [] monoFont ovs
                 g x al Nothing = Just (x, x + length (attrLine al) - 1)
                 g x al (Just (xmin, xmax)) =
                   Just (min xmin x, max xmax (x + length (attrLine al) - 1))
                 f em (K.PointUI x y, al) = EM.alter (g x al) y em
                 extentMap = foldl' f EM.empty $ propOutline ++ monoOutline
                 listBackdrop (y, (xmin, xmax)) =
                   ( K.PointUI (2 * (xmin `div` 2)) y
                   , attrStringToAL
                     $ blankAttrString
                       $ min (rwidth - 2 * (xmin `div` 2))
                             (1 + xmax `divUp` 2 - xmin `div` 2) )
             in map listBackdrop $ EM.assocs extentMap
        else []
      overlayedFrame = overlayFrame rwidth ovOther
                       $ overlayFrame rwidth ovBackdrop basicFrame
  return (overlayedFrame, (ovProp, ovMono))

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
    FontSetup{propFont} <- getFontSetup
    let par1 = paragraph1OfAS $ renderReport report
        truncRep = EM.fromList [(propFont, [(K.PointUI 0 0, par1)])]
    frame <- drawOverlay ColorFull False truncRep lidV
    displayFrames lidV [Just frame]

promptGetKey :: MonadClientUI m
             => ColorMode -> FontOverlayMap -> Bool -> [K.KM]
             -> m K.KM
promptGetKey dm ovs onBlank frontKeyKeys = do
  lidV <- viewedLevelUI
  keyPressed <- anyKeyPressed
  report <- getsSession $ newReport . shistory
  let msgDisturbs = anyInReport disturbsResting report
  lastPlayOld <- getsSession slastPlay
  km <- case lastPlayOld of
    KeyMacro (km : kms) | not keyPressed
                          && (null frontKeyKeys || km `elem` frontKeyKeys)
                          && not msgDisturbs -> do
      frontKeyFrame <- drawOverlay dm onBlank ovs lidV
      displayFrames lidV [Just frontKeyFrame]
      modifySession $ \sess -> sess {slastPlay = KeyMacro kms}
      msgAdd MsgMacro $ "Voicing '" <> tshow km <> "'."
      return km
    KeyMacro (_ : _) -> do
      -- We can't continue playback, so wipe out old slastPlay, srunning, etc.
      resetPlayBack
      resetPressedKeys
      FontSetup{propFont} <- getFontSetup
      let ovWarn = [ (K.PointUI 0 0, textFgToAL Color.BrYellow "*interrupted*")
                   | keyPressed ]
          ovs2 = EM.insertWith (++) propFont ovWarn ovs
      frontKeyFrame <- drawOverlay dm onBlank ovs2 lidV
      recordHistory
      connFrontendFrontKey frontKeyKeys frontKeyFrame
    KeyMacro [] -> do
      -- If we ask for a key, then we don't want to run any more
      -- and we want to avoid changing leader back to initial run leader
      -- at the nearest @stopPlayBack@, etc.
      modifySession $ \sess -> sess {srunning = Nothing}
      frontKeyFrame <- drawOverlay dm onBlank ovs lidV
      when (dm /= ColorFull) $ do
        side <- getsClient sside
        fact <- getsState $ (EM.! side) . sfactionD
        unless (isAIFact fact) -- don't forget special autoplay keypresses
          -- Forget the furious keypresses just before a special event.
          resetPressedKeys
      recordHistory
      connFrontendFrontKey frontKeyKeys frontKeyFrame
  -- In-game macros need to be recorded here, not in @UI.humanCommand@,
  -- to also capture choice of items from menus, etc.
  -- Notice that keys coming from macros (in-game, content, config)
  -- are recorded as well and this is well defined and essential.
  CCUI{coinput=InputContent{brevMap}} <- getsSession sccui
  modifySession $ \sess ->
    sess { sdisplayNeeded = False
         , sturnDisplayed = True
         , smacroStack = addToMacro brevMap km $ smacroStack sess }
  return km

addToMacro :: M.Map HumanCmd.HumanCmd [K.KM] -> K.KM -> MacroStack -> MacroStack
addToMacro brevMap km macros =
  let mKeyRecord = case M.lookup HumanCmd.Record brevMap of
        Nothing -> Nothing
        Just (k : _) -> Just k
        Just [] -> error $ "" `showFailure` brevMap
  in if Just km == mKeyRecord
     then macros
       -- Exclude from in-game macros keystrokes that
       -- start/stop recording a macro.
     else case macros of
       -- This is noop when not recording a macro,
       -- which is exactly the required semantics.
       Left xs : ys -> Left (km : xs) : ys
       -- Record keystrokes in the first Left buffer on the stack;
       -- there's at most one Right temporary macro buffer.
       Right xs : (Left ys : zs) -> Right xs : (Left (km : ys) : zs)
       _ -> macros

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = msgAdd0 MsgStopPlayback "!"

resetPlayBack :: MonadClientUI m => m ()
resetPlayBack = do
  lastPlayOld <- getsSession slastPlay
  unless (lastPlayOld == mempty)
    $ modifySession $ \sess ->
       sess { slastPlay = mempty
            , smacroStack = case smacroStack sess of
                -- If interrupted, leave on stack user's macro buffer
                -- (it's always on the bottom) and pop all the rest
                -- of temporary macro buffers.
                [_] -> smacroStack sess
                _ -> [last $ smacroStack sess] }
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
      memA <- getsState $ memActor runLeader arena
      when (memA && not (noRunWithMulti fact)) $
        updateClientLeader runLeader
      modifySession (\sess -> sess {srunning = Nothing})

-- | Render animations on top of the current screen frame.
renderFrames :: MonadClientUI m => Bool -> LevelId -> Animation -> m PreFrames3
renderFrames onBlank arena anim = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  snoAnim <- getsClient $ snoAnim . soptions
  report <- getReportUI
  FontSetup{..} <- getFontSetup
  let ovFont = if not onBlank || fromMaybe False snoAnim
               then propFont
               else squareFont
      par1 = paragraph1OfAS $ renderReport report
      truncRep = EM.fromList [(ovFont, [(K.PointUI 0 0, par1)])]
  basicFrame <- drawOverlay ColorFull False truncRep arena
  return $! if fromMaybe False snoAnim
            then [Just basicFrame]
            else map (fmap (\fr -> (fr, snd basicFrame)))
                 $ renderAnim rwidth (fst basicFrame) anim

-- | Render and display animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m ()
animate arena anim = do
  -- The delay before reaction to keypress was too long in case of many
  -- projectiles hitting actors, so frames need to be skipped.
  keyPressed <- anyKeyPressed
  unless keyPressed $ do
    frames <- renderFrames False arena anim
    displayFrames arena frames

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  arena <- getArenaUI
  CCUI{coscreen} <- getsSession sccui
  animMap <- rndToActionUI $ fadeout coscreen out 2
  animFrs <- renderFrames True arena animMap
  displayFrames arena (tail animFrs)  -- no basic frame between fadeout and in
