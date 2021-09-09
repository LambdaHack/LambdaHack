-- | A set of Frame monad operations.
module Game.LambdaHack.Client.UI.FrameM
  ( pushFrame, promptGetKey, addToMacro, dropEmptyMacroFrames, lastMacroFrame
  , stopPlayBack, animate, fadeOutOrIn
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , drawOverlay, oneLineBasicFrame, renderAnimFrames, resetPlayBack
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Bifunctor as B
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U

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
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions
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
      propWidth = if isMonoFont propFont then 2 * rwidth else 4 * rwidth
      ovProp | not $ isSquareFont propFont
             = truncateOverlay False propWidth rheight False 0 onBlank
               $ EM.findWithDefault [] propFont ovs
             | isTeletype  -- hack for debug output
             = map (second attrLine) $ concat $ EM.elems ovs
             | otherwise = []
      ovMono = if not $ isSquareFont monoFont
               then truncateOverlay False (2 * rwidth) rheight False 0 onBlank
                    $ EM.findWithDefault [] monoFont ovs
               else []
      ovOther | not $ isSquareFont propFont
              = truncateOverlay True rwidth rheight True 0 onBlank
                $ EM.findWithDefault [] squareFont ovs
                  -- no backdrop for square font, so @wipeAdjacent@
                  -- needs to be @True@ or the extra blank line starts too late
              | isTeletype  -- hack for debug output
              = []
              | otherwise
              = truncateOverlay True rwidth rheight True 20 onBlank
                $ concat $ EM.elems ovs
                    -- 20 needed not to leave gaps in skill menu
                    -- in the absence of backdrop
      ovBackdrop =
        if not (isSquareFont propFont) && not onBlank
        then let propOutline =
                   truncateOverlay False propWidth rheight True 0 onBlank
                   $ EM.findWithDefault [] propFont ovs
                 monoOutline =
                   truncateOverlay False (2 * rwidth) rheight True 0 onBlank
                   $ EM.findWithDefault [] monoFont ovs
                 g x al Nothing = Just (x, x + length al - 1)
                 g x al (Just (xmin, xmax)) =
                   Just (min xmin x, max xmax (x + length al - 1))
                 f em (PointUI x y, al) = EM.alter (g x al) y em
                 extentMap = foldl' f EM.empty $ propOutline ++ monoOutline
                 listBackdrop (y, (xmin, xmax)) =
                   ( PointUI (2 * (xmin `div` 2)) y
                   , blankAttrString
                     $ min (rwidth - 2 * (xmin `div` 2))
                           (1 + xmax `divUp` 2 - xmin `div` 2) )
             in map listBackdrop $ EM.assocs extentMap
        else []
      overlayedFrame = overlayFrame rwidth ovOther
                       $ overlayFrame rwidth ovBackdrop basicFrame
  return (overlayedFrame, (ovProp, ovMono))

oneLineBasicFrame :: MonadClientUI m => LevelId -> DisplayFont -> m PreFrame3
oneLineBasicFrame arena font = do
  report <- getReportUI False
  let par1 = firstParagraph $ foldr (<+:>) [] $ renderReport True report
      truncRep = EM.fromList [(font, [(PointUI 0 0, par1)])]
  drawOverlay ColorFull False truncRep arena

-- | Push the frame depicting the current level to the frame queue.
-- Only one line of the report is shown, as in animations,
-- because it may not be our turn, so we can't clear the message
-- to see what is underneath.
pushFrame :: MonadClientUI m => Bool -> m ()
pushFrame delay = do
  -- The delay before reaction to keypress was too long in case of many
  -- projectiles flying and ending flight, so frames need to be skipped.
  keyPressed <- anyKeyPressed
  unless keyPressed $ do
    lidV <- viewedLevelUI
    FontSetup{propFont} <- getFontSetup
    frame <- oneLineBasicFrame lidV propFont
    -- Pad with delay before and after to let player see, e.g., door being
    -- opened a few ticks after it came into vision, the same turn.
    displayFrames lidV $
      if delay then [Nothing, Just frame, Nothing] else [Just frame]

promptGetKey :: MonadClientUI m
             => ColorMode -> FontOverlayMap -> Bool -> [K.KM]
             -> m K.KM
promptGetKey dm ovs onBlank frontKeyKeys = do
  lidV <- viewedLevelUI
  report <- getsSession $ newReport . shistory
  let interrupted = anyInReport disturbsResting report
  macroFrame <- getsSession smacroFrame
  km <- case keyPending macroFrame of
    KeyMacro (km : kms) | (null frontKeyKeys || km `elem` frontKeyKeys)
                          && not interrupted -> do
      -- No need to display the frame, because a frame was displayed
      -- when the player chose to play a macro and each turn or more often
      -- a frame is displayed elsewhere.
      -- The only excepton is when navigating menus through macros,
      -- but there the speed is particularly welcome.
      modifySession $ \sess ->
        sess {smacroFrame = (smacroFrame sess) {keyPending = KeyMacro kms}}
      msgAdd MsgMacroOperation $ "Voicing '" <> tshow km <> "'."
      return km
    KeyMacro (_ : _) -> do
      frontKeyFrame <- drawOverlay dm onBlank ovs lidV
      -- We can't continue playback, so wipe out macros, etc.
      resetPlayBack
      resetPressedKeys
      recordHistory
      modifySession $ \sess ->
        sess { sdisplayNeeded = False
             , sturnDisplayed = True }
      connFrontendFrontKey frontKeyKeys frontKeyFrame
    KeyMacro [] -> do
      frontKeyFrame <- drawOverlay dm onBlank ovs lidV
      when (dm /= ColorFull) $ do
        side <- getsClient sside
        fact <- getsState $ (EM.! side) . sfactionD
        unless (isAIFact fact) -- don't forget special autoplay keypresses
          -- Forget the furious keypresses just before a special event.
          resetPressedKeys
      recordHistory
      -- If we ask for a key, then we don't want to run any more
      -- and we want to avoid changing leader back to initial run leader
      -- at the nearest @stopPlayBack@, etc.
      modifySession $ \sess ->
        sess { srunning = Nothing
             , sdisplayNeeded = False
             , sturnDisplayed = True }
      connFrontendFrontKey frontKeyKeys frontKeyFrame
  -- In-game macros need to be recorded here, not in @UI.humanCommand@,
  -- to also capture choice of items from menus, etc.
  -- Notice that keys coming from macros (from content, in-game, config)
  -- are recorded as well and this is well defined and essential.
  CCUI{coinput=InputContent{bcmdMap}} <- getsSession sccui
  modifySession $ \sess ->
    sess {smacroFrame = addToMacro bcmdMap km $ smacroFrame sess}
  return km

addToMacro :: M.Map K.KM HumanCmd.CmdTriple -> K.KM -> KeyMacroFrame
           -> KeyMacroFrame
addToMacro bcmdMap km macroFrame =
  case (\(_, _, cmd) -> cmd) <$> M.lookup km bcmdMap of
    Nothing -> macroFrame
    Just HumanCmd.Record -> macroFrame
    Just HumanCmd.RepeatLast{} -> macroFrame
    _ -> macroFrame { keyMacroBuffer =
                        (km :) `B.first` keyMacroBuffer macroFrame }
           -- This is noop when not recording a macro,
           -- which is exactly the required semantics.

dropEmptyMacroFrames :: KeyMacroFrame -> [KeyMacroFrame]
                     -> (KeyMacroFrame, [KeyMacroFrame])
dropEmptyMacroFrames mf [] = (mf, [])
dropEmptyMacroFrames (KeyMacroFrame _ (KeyMacro []) _)
                     (mf : mfs) = dropEmptyMacroFrames mf mfs
dropEmptyMacroFrames mf mfs = (mf, mfs)

lastMacroFrame :: KeyMacroFrame -> [KeyMacroFrame] -> KeyMacroFrame
lastMacroFrame mf [] = mf
lastMacroFrame _ (mf : mfs) = lastMacroFrame mf mfs

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = msgAdd MsgStopPlayback "!"

resetPlayBack :: MonadClientUI m => m ()
resetPlayBack = do
  -- We wipe any actions in progress, but keep the data needed to repeat
  -- the last global macros and the last command.
  modifySession $ \sess ->
    let lastFrame = lastMacroFrame (smacroFrame sess) (smacroStack sess)
    in sess { smacroFrame = lastFrame {keyPending = mempty}
            , smacroStack = []
            , sxhairGoTo = Nothing }
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
renderAnimFrames :: MonadClientUI m
                 => Bool -> LevelId -> Animation -> m PreFrames3
renderAnimFrames onBlank arena anim = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  snoAnim <- getsClient $ snoAnim . soptions
  FontSetup{..} <- getFontSetup
  -- This hack is needed so that the prop part of the overlay does not
  -- overwrite the fadeout animation.
  let ovFont = if not onBlank || fromMaybe False snoAnim
               then propFont
               else squareFont
  basicFrame <- oneLineBasicFrame arena ovFont
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
    frames <- renderAnimFrames False arena anim
    displayFrames arena frames

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  arena <- getArenaUI
  CCUI{coscreen} <- getsSession sccui
  animMap <- rndToActionUI $ fadeout coscreen out 2
  animFrs <- renderAnimFrames True arena animMap
  displayFrames arena (tail animFrs)  -- no basic frame between fadeout and in
