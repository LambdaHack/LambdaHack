-- | A set of Frame monad operations.
module Game.LambdaHack.Client.UI.FrameM
  ( drawOverlay, promptGetKey, addToMacro, dropEmptyMacroFrames
  , lastMacroFrame, stopPlayBack, renderAnimFrames, animate
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , resetPlayBack, restoreLeaderFromRun, basicFrameForAnimation
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
  let propWidth = if propFont == MonoFont then 2 * rwidth else 4 * rwidth
      ovProp | propFont /= SquareFont
             = truncateOverlay False propWidth rheight False 0 onBlank
               $ EM.findWithDefault [] propFont ovs
             | otherwise = []
      ovMono = if monoFont /= SquareFont
               then truncateOverlay False (2 * rwidth) rheight False 0 onBlank
                    $ EM.findWithDefault [] monoFont ovs
               else []
      ovSquare | propFont /= SquareFont
               = truncateOverlay False (2 * rwidth) rheight False 0 onBlank
                 $ EM.findWithDefault [] squareFont ovs
              | otherwise = []
      ovOther | propFont /= SquareFont = []
              | otherwise
              = truncateOverlay True rwidth rheight True 20 onBlank
                $ concat $ EM.elems ovs
                    -- 20 needed not to leave gaps in skill menu
                    -- in the absence of backdrop
      ovBackdrop =
        if propFont /= SquareFont && not onBlank
        then let propOutline =
                   truncateOverlay False propWidth rheight True 0 onBlank
                   $ EM.findWithDefault [] propFont ovs
                 monoOutline =
                   truncateOverlay False (2 * rwidth) rheight True 0 onBlank
                   $ EM.findWithDefault [] monoFont ovs
                 squareOutline =
                   truncateOverlay False (2 * rwidth) rheight True 0 onBlank
                   $ EM.findWithDefault [] squareFont ovs
                 g x al Nothing = Just (x, x + length al - 1)
                 g x al (Just (xmin, xmax)) =
                   Just (min xmin x, max xmax (x + length al - 1))
                 f em (PointUI x y, al) = EM.alter (g x al) y em
                 extentMap = foldl' f EM.empty
                             $ propOutline ++ monoOutline ++ squareOutline
                 listBackdrop (y, (xmin, xmax)) =
                   ( PointUI (2 * (xmin `div` 2)) y
                   , blankAttrString
                     $ min (rwidth - 2 * (xmin `div` 2))
                           (1 + xmax `divUp` 2 - xmin `div` 2) )
             in map listBackdrop $ EM.assocs extentMap
        else []
      overlayedFrame = overlayFrame rwidth ovOther
                       $ overlayFrame rwidth ovBackdrop basicFrame
  return (overlayedFrame, (ovProp, ovSquare, ovMono))

promptGetKey :: MonadClientUI m
             => ColorMode -> FontOverlayMap -> Bool -> [K.KM]
             -> m K.KM
promptGetKey dm ovs onBlank frontKeyKeys = do
  lidV <- viewedLevelUI
  report <- getsSession $ newReport . shistory
  sreqQueried <- getsSession sreqQueried
  let interrupted =
        -- If server is not querying for request, then the key is needed due to
        -- a special event, not ordinary querying the player for command,
        -- so interrupt.
        not sreqQueried
        -- Any alarming message interupts macros.
        || anyInReport disturbsResting report
  macroFrame <- getsSession smacroFrame
  km <- case keyPending macroFrame of
    KeyMacro (km : kms) | not interrupted
                          -- A faulty key in a macro is a good reason
                          -- to interrupt it, as well.
                          && (null frontKeyKeys || km `elem` frontKeyKeys) -> do
      -- No need to display the frame, because a frame was displayed
      -- when the player chose to play a macro and each turn or more often
      -- a frame is displayed elsewhere.
      -- The only excepton is when navigating menus through macros,
      -- but there the speed is particularly welcome.
      modifySession $ \sess ->
        sess {smacroFrame = (smacroFrame sess) {keyPending = KeyMacro kms}}
      msgAdd MsgMacroOperation $ "Voicing '" <> tshow km <> "'."
      return km
    KeyMacro kms -> do
      if null kms then do
        -- There was no macro. Not important if there was a reason
        -- for interrupt or not.
        when (dm /= ColorFull) $ do
          -- This marks a special event, regardless of @sreqQueried@.
          side <- getsClient sside
          fact <- getsState $ (EM.! side) . sfactionD
          unless (isAIFact fact) -- don't forget special autoplay keypresses
            -- Forget the furious keypresses just before a special event.
            resetPressedKeys
        -- Running, if any, must have ended naturally, because no macro.
        -- Therefore no need to restore leader back to initial run leader,
        -- but running itself is cancelled below.
      else do
        -- The macro was not empty, but not played, so it must have been
        -- interrupted, so we can't continue playback, so wipe out the macro.
        resetPlayBack
        -- This might have been an unexpected end of a run, too.
        restoreLeaderFromRun
        -- Macro was killed, so emergency, so reset input, too.
        resetPressedKeys
      frontKeyFrame <- drawOverlay dm onBlank ovs lidV
      recordHistory
      modifySession $ \sess ->
        sess { srunning = Nothing
             , sxhairGoTo = Nothing
             , sdisplayNeeded = False
             , sturnDisplayed = True }
      connFrontendFrontKey frontKeyKeys frontKeyFrame
  -- In-game macros need to be recorded here, not in @UI.humanCommand@,
  -- to also capture choice of items from menus, etc.
  -- Notice that keys coming from macros (from content, in-game, config)
  -- are recorded as well and this is well defined and essential.
  --
  -- Only keys pressed when player is queried for a command are recorded.
  when sreqQueried $ do
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

-- | We wipe any actions in progress, but keep the data needed to repeat
-- the last global macros and the last command.
resetPlayBack :: MonadClientUI m => m ()
resetPlayBack =
  modifySession $ \sess ->
    let lastFrame = lastMacroFrame (smacroFrame sess) (smacroStack sess)
    in sess { smacroFrame = lastFrame {keyPending = mempty}
            , smacroStack = [] }

restoreLeaderFromRun :: MonadClientUI m => m ()
restoreLeaderFromRun = do
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

-- This is not our turn, so we can't obstruct screen with messages
-- and message reformatting causes distraction, so there's no point
-- trying to squeeze the report into the single available line,
-- except when it's not our turn permanently, because AI runs UI.
basicFrameForAnimation :: MonadClientUI m
                        => LevelId -> Maybe Bool -> m PreFrame3
basicFrameForAnimation arena forceReport = do
  FontSetup{propFont} <- getFontSetup
  sbenchMessages <- getsClient $ sbenchMessages . soptions
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  report <- getReportUI False
  let par1 = firstParagraph $ foldr (<+:>) [] $ renderReport True report
      underAI = isAIFact fact
      -- If messages are benchmarked, they can't be displayed under AI,
      -- because this is not realistic when player is in control.
      truncRep | not sbenchMessages && fromMaybe underAI forceReport =
                   EM.fromList [(propFont, [(PointUI 0 0, par1)])]
               | otherwise = EM.empty
  drawOverlay ColorFull False truncRep arena

-- | Render animations on top of the current screen frame.
renderAnimFrames :: MonadClientUI m
                 => LevelId -> Animation -> Maybe Bool -> m PreFrames3
renderAnimFrames arena anim forceReport = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  snoAnim <- getsClient $ snoAnim . soptions
  basicFrame <- basicFrameForAnimation arena forceReport
  smuteMessages <- getsSession smuteMessages
  return $! if | smuteMessages -> []
               | fromMaybe False snoAnim -> [Just basicFrame]
               | otherwise -> map (fmap (\fr -> (fr, snd basicFrame)))
                              $ renderAnim rwidth (fst basicFrame) anim

-- | Render and display animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m ()
animate arena anim = do
  -- The delay before reaction to keypress was too long in case of many
  -- projectiles hitting actors, so frames need to be skipped.
  keyPressed <- anyKeyPressed
  unless keyPressed $ do
    frames <- renderAnimFrames arena anim Nothing
    displayFrames arena frames
