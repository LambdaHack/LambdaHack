-- | A set of widgets for UI clients.
module Game.LambdaHack.Client.UI.WidgetClient
  ( displayMore, displayYesNo, displayChoiceUI, displayPush, displayPushIfLid
  , promptToSlideshow, overlayToSlideshow, overlayToBlankSlideshow
  , animate, fadeOutOrIn
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Monoid

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient hiding (liftIO)
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.DrawClient
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

-- | A yes-no confirmation.
getYesNo :: MonadClientUI m => SingleFrame -> m Bool
getYesNo frame = do
  let keys = [ K.KM {key=K.Char 'y', modifier=K.NoModifier}
             , K.KM {key=K.Char 'n', modifier=K.NoModifier}
             , K.escKM
             ]
  K.KM {key} <- promptGetKey keys frame
  case key of
    K.Char 'y' -> return True
    _          -> return False

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayMore dm prompt = do
  slides <- promptToSlideshow $ prompt <+> moreMsg
  -- Two frames drawn total (unless 'prompt' very long).
  getInitConfirms dm [] $ slides <> toSlideshow False [[]]

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayYesNo dm prompt = do
  sli <- promptToSlideshow $ prompt <+> yesnoMsg
  frame <- drawOverlay False dm $ head . snd $ slideshow sli
  getYesNo frame

-- TODO: generalize getInitConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: MonadClientUI m
                => Msg -> Overlay -> [K.KM] -> m (Either Slideshow K.KM)
displayChoiceUI prompt ov keys = do
  (_, ovs) <- fmap slideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  let legalKeys = [K.spaceKM, K.escKM]
        ++ keys
      loop [] = fmap Left $ promptToSlideshow "never mind"
      loop (x : xs) = do
        frame <- drawOverlay False ColorFull x
        km@K.KM {..} <- promptGetKey legalKeys frame
        case key of
          K.Esc -> fmap Left $ promptToSlideshow "never mind"
          K.Space -> loop xs
          _ -> return $ Right km
  loop ovs

-- TODO: if more slides, don't take head, but do as in getInitConfirms,
-- but then we have to clear the messages or they get redisplayed
-- each time screen is refreshed.
-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadClientUI m => m ()
displayPush = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  sls <- promptToSlideshow ""
  let slide = head . snd $ slideshow sls
      hasAiLeader = playerAiLeader $ gplayer fact
  frame <- drawOverlay False ColorFull slide
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  srunning <- getsClient srunning
  lastPlay <- getsClient slastPlay
  displayFrame (isJust srunning || not (null lastPlay) || hasAiLeader)
               (Just frame)

displayPushIfLid :: MonadClientUI m => LevelId -> m ()
displayPushIfLid lid = do
  arena <- getArenaUI
  when (arena == lid) displayPush

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadClientUI m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt emptyOverlay

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  sreport <- getsClient sreport
  let msg = splitReport lxsize (addMsg sreport prompt)
  return $! splitOverlay False (lysize + 1) msg overlay

overlayToBlankSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToBlankSlideshow prompt overlay = do
  lid <- getArenaUI
  Level{lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  return $! splitOverlay True (lysize + 3) (toOverlay [prompt]) overlay

-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m Frames
animate arena anim = do
  cops <- getsState scops
  sreport <- getsClient sreport
  mleader <- getsClient _sleader
  Level{lxsize, lysize} <- getLevel arena
  cli <- getClient
  s <- getState
  per <- getPerFid arena
  tgtPos <- leaderTgtToPos
  cursorPos <- cursorToPos
  let anyPos = fromMaybe (Point 0 0) cursorPos
      pathFromLeader leader = fmap Just $ getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return ("------", Nothing)) targetDescLeader mleader
  cursorDesc <- targetDescCursor
  let over = renderReport sreport
      topLineOnly = truncateToOverlay lxsize over
      basicFrame =
        draw False ColorFull cops per arena mleader
             cursorPos tgtPos bfsmpath cli s cursorDesc tgtDesc topLineOnly
  snoAnim <- getsClient $ snoAnim . sdebugCli
  return $! if fromMaybe False snoAnim
            then [Just basicFrame]
            else renderAnim lxsize lysize basicFrame anim

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  let topRight = True
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid
  animMap <- rndToAction $ fadeout out topRight 2 lxsize lysize
  animFrs <- animate lid animMap
  displayFrames animFrs
