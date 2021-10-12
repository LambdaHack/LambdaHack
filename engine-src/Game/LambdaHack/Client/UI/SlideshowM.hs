-- | Monadic operations on slideshows and related data.
module Game.LambdaHack.Client.UI.SlideshowM
  ( overlayToSlideshow, reportToSlideshow, reportToSlideshowKeepHalt
  , displaySpaceEsc, displayMore, displayMoreKeep, displayYesNo, getConfirms
  , displayChoiceScreen, displayChoiceScreenWithRightPane
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , getMenuIx, saveMenuIx, stepChoiceScreen, navigationKeys, findKYX
  , drawHighlight
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.UIOptions
import qualified Game.LambdaHack.Definition.Color as Color

-- | Add current report to the overlay, split the result and produce,
-- possibly, many slides.
overlayToSlideshow :: MonadClientUI m
                   => Int -> [K.KM] -> OKX -> m Slideshow
overlayToSlideshow y keys okx = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  UIOptions{uMsgWrapColumn} <- getsSession sUIOptions
  report <- getReportUI True
  recordHistory  -- report will be shown soon, remove it to history
  fontSetup <- getFontSetup
  return $! splitOverlay fontSetup rwidth y uMsgWrapColumn report keys okx

-- | Split current report into a slideshow.
reportToSlideshow :: MonadClientUI m => [K.KM] -> m Slideshow
reportToSlideshow keys = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  overlayToSlideshow (rheight - 2) keys emptyOKX

-- | Split current report into a slideshow. Keep report unchanged.
-- Assume the game either halts waiting for a key after this is shown,
-- or many slides are produced, all but the last are displayed
-- with player promts between and the last is either shown
-- in full or ignored if inside macro (can be recovered from history,
-- if important). Unless the prompts interrupt the macro, which is as well.
reportToSlideshowKeepHalt :: MonadClientUI m => Bool -> [K.KM] -> m Slideshow
reportToSlideshowKeepHalt insideMenu keys = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  UIOptions{uMsgWrapColumn} <- getsSession sUIOptions
  report <- getReportUI insideMenu
  -- Don't do @recordHistory@; the message is important, but related
  -- to the messages that come after, so should be shown together.
  fontSetup <- getFontSetup
  return $! splitOverlay fontSetup rwidth (rheight - 2) uMsgWrapColumn
                         report keys emptyOKX

-- | Display a message. Return value indicates if the player wants to continue.
-- Feature: if many pages, only the last SPACE exits (but first ESC).
displaySpaceEsc :: MonadClientUI m => ColorMode -> Text -> m Bool
displaySpaceEsc dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  -- Two frames drawn total (unless @prompt@ very long).
  slides <- reportToSlideshow [K.spaceKM, K.escKM]
  km <- getConfirms dm [K.spaceKM, K.escKM] slides
  return $! km == K.spaceKM

-- | Display a message. Ignore keypresses.
-- Feature: if many pages, only the last SPACE exits (but first ESC).
displayMore :: MonadClientUI m => ColorMode -> Text -> m ()
displayMore dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  slides <- reportToSlideshow [K.spaceKM]
  void $ getConfirms dm [K.spaceKM, K.escKM] slides

displayMoreKeep :: MonadClientUI m => ColorMode -> Text -> m ()
displayMoreKeep dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  slides <- reportToSlideshowKeepHalt True [K.spaceKM]
  void $ getConfirms dm [K.spaceKM, K.escKM] slides

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Text -> m Bool
displayYesNo dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  let yn = map K.mkChar ['y', 'n']
  slides <- reportToSlideshow yn
  km <- getConfirms dm (K.escKM : yn) slides
  return $! km == K.mkChar 'y'

getConfirms :: MonadClientUI m
            => ColorMode -> [K.KM] -> Slideshow -> m K.KM
getConfirms dm extraKeys slides = do
  ekm <- displayChoiceScreen "" dm False slides extraKeys
  return $! either id (error $ "" `showFailure` ekm) ekm

-- | Display a, potentially, multi-screen menu and return the chosen
-- key or item slot label (and save the index in the whole menu so that the cursor
-- can again be placed at that spot next time menu is displayed).
--
-- This function is one of only two sources of menus and so,
-- effectively, UI modes.
displayChoiceScreen :: forall m . MonadClientUI m
                    => String -> ColorMode -> Bool -> Slideshow -> [K.KM]
                    -> m KeyOrSlot
displayChoiceScreen =
  displayChoiceScreenWithRightPane $ const $ return emptyOKX

-- | Display a, potentially, multi-screen menu and return the chosen
-- key or item slot label (and save the index in the whole menu so that the cursor
-- can again be placed at that spot next time menu is displayed).
-- Additionally, display something on the right half of the screen,
-- depending on which menu item is currently highlighted
--
-- This function is one of only two sources of menus and so,
-- effectively, UI modes.
displayChoiceScreenWithRightPane
  :: forall m . MonadClientUI m
  => (KeyOrSlot -> m OKX)
  -> String -> ColorMode -> Bool -> Slideshow -> [K.KM]
  -> m KeyOrSlot
displayChoiceScreenWithRightPane displayInRightPane
                                 menuName dm sfBlank frsX extraKeys = do
  FontSetup{propFont} <- getFontSetup
  let displayInRightUnlessSquare km = if isSquareFont propFont
                                      then return emptyOKX
                                      else displayInRightPane km
  (maxIx, initIx, clearIx, m) <-
    stepChoiceScreen menuName dm sfBlank frsX extraKeys
  let loop :: Int -> KeyOrSlot -> m (KeyOrSlot, Int)
      loop pointer km = do
        okxRight <- displayInRightUnlessSquare km
        (final, km1, pointer1) <- m pointer okxRight
        if final
        then return (km1, pointer1)
        else loop pointer1 km1
  pointer0 <- getMenuIx menuName maxIx initIx clearIx
  let km0 = case findKYX pointer0 $ slideshow frsX of
        Nothing -> error $ "no menu keys" `showFailure` frsX
        Just (_, (ekm, _), _) -> ekm
  (km, pointer) <- loop pointer0 km0
  saveMenuIx menuName initIx pointer
  return km

getMenuIx :: MonadClientUI m => String -> Int -> Int -> Int -> m Int
getMenuIx menuName maxIx initIx clearIx = do
  menuIxMap <- getsSession smenuIxMap
  -- Beware, values in @menuIxMap@ may be negative (meaning: a key, not slot).
  let menuIx = if menuName == ""
               then clearIx
               else maybe clearIx (+ initIx) (M.lookup menuName menuIxMap)
                      -- this may still be negative, from different context
  return $! max clearIx $ min maxIx menuIx  -- so clamp to point at item, not key

saveMenuIx :: MonadClientUI m => String -> Int -> Int -> m ()
saveMenuIx menuName initIx pointer =
  unless (menuName == "") $
    modifySession $ \sess ->
      sess {smenuIxMap = M.insert menuName (pointer - initIx) $ smenuIxMap sess}

-- | This is one step of UI menu management user session.
--
-- There is limited looping involved to return a changed position
-- in the menu each time so that the surrounding code has anything
-- interesting to do. The exception is when finally confirming a selection,
-- in which case it's usually not changed compared to last step,
-- but it's presented differently to indicate it was confirmed.
--
-- Any extra keys in the `OKX` argument on top of the those in @Slideshow@
-- argument need to be contained in the @[K.KM]@ argument. Otherwise
-- they are not accepted.
stepChoiceScreen :: forall m . MonadClientUI m
                 => String -> ColorMode -> Bool -> Slideshow -> [K.KM]
                 -> m ( Int, Int, Int
                      , Int -> OKX -> m (Bool, KeyOrSlot, Int) )
stepChoiceScreen menuName dm sfBlank frsX extraKeys = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  FontSetup{..} <- getFontSetup
  let !_A = assert (K.escKM `elem` extraKeys) ()
      frs = slideshow frsX
      keys = concatMap (lefts . map fst . snd) frs ++ extraKeys
      legalKeys = keys
                  ++ navigationKeys
                  ++ [K.mkChar '?' | menuName == "help"]  -- a hack
      maxIx = length (concatMap snd frs) - 1
      allOKX = concatMap snd frs
      initIx = case findIndex (isRight . fst) allOKX of
        Just p -> p
        _ -> 0  -- can't be @length allOKX@ or a multi-page item menu
                -- mangles saved index of other item munus
      clearIx = if initIx > maxIx then 0 else initIx
      canvasLength = if sfBlank then rheight else rheight - 2
      trimmedY = canvasLength - 1 - 2  -- will be translated down 2 lines
      trimmedAlert = ( PointUI 0 trimmedY
                     , stringToAL "--a portion of the text trimmed--" )
      page :: Int -> OKX -> m (Bool, KeyOrSlot, Int)
      page pointer (ovsRight0, kyxsRight) = assert (pointer >= 0)
                                            $ case findKYX pointer frs of
        Nothing -> error $ "no menu keys" `showFailure` frs
        Just ( (ovs0, kyxs1)
             , (ekm, (PointUI x1 y, buttonWidth))
             , ixOnPage ) -> do
          let ovs1 = EM.map (updateLine y $ drawHighlight x1 buttonWidth) ovs0
              -- We add spaces in proportional font under the report rendered
              -- in mono font and the right pane text in prop font,
              -- but over menu lines in proportional font that can be
              -- very long an should not peek from under the right pane text.
              --
              -- We translate the pane by two characters right, because it looks
              -- better when a couple last characters of a line vanish
              -- off-screen than when characters touch in the middle
              -- of the screen. The code producing right panes should take care
              -- to generate lines two shorter than usually.
              --
              -- We move the pane two characters down, because normally
              -- reports should not be longer than three lines
              -- and the third no longer than half width.
              -- We also add two to three lines of backdrop at the bottom.
              ymax = maxYofFontOverlayMap ovsRight0
              spaceRectangle = rectangleOfSpaces (rwidth * 2)
                                                 (min canvasLength $ ymax + 5)
              trim = filter (\(PointUI _ yRight, _) -> yRight < trimmedY)
              -- The alert not clickable, because the player can enter
              -- the menu entry and scroll through the unabridged blurb.
              ovsRight1 = if ymax <= trimmedY
                          then ovsRight0
                          else EM.unionWith (++)
                                 (EM.map trim ovsRight0)
                                 (EM.singleton monoFont [trimmedAlert])
              ovsRight = EM.unionWith (++)
                          (EM.singleton propFont spaceRectangle)
                          (EM.map (xytranslateOverlay 2 2) ovsRight1)
              (ovs, kyxs) =
                if EM.null ovsRight0
                then (ovs1, kyxs1)
                else sideBySideOKX rwidth 0 (ovs1, kyxs1) (ovsRight, kyxsRight)
              tmpResult pointer1 = case findKYX pointer1 frs of
                Nothing -> error $ "no menu keys" `showFailure` frs
                Just (_, (ekm1, _), _) -> return (False, ekm1, pointer1)
              ignoreKey = return (False, ekm, pointer)
              pageLen = length kyxs
              xix :: KYX -> Bool
              xix (_, (PointUI x1' _, _)) = x1' <= x1 + 2 && x1' >= x1 - 2
              firstRowOfNextPage = pointer + pageLen - ixOnPage
              restOKX = drop firstRowOfNextPage allOKX
              -- This does not take into account the right pane, which is fine.
              firstItemOfNextPage = case findIndex (isRight . fst) restOKX of
                Just p -> p + firstRowOfNextPage
                _ -> firstRowOfNextPage
              interpretKey :: K.KM -> m (Bool, KeyOrSlot, Int)
              interpretKey ikm =
                case K.key ikm of
                  _ | ikm == K.controlP -> do
                    -- Silent, because any prompt would be shown too late.
                    printScreen
                    ignoreKey
                  K.Return -> case ekm of
                    Left km ->
                      if K.key km == K.Return && km `elem` keys
                      then return (True, Left km, pointer)
                      else interpretKey km
                    Right c -> return (True, Right c, pointer)
                  K.LeftButtonRelease -> do
                    PointUI mx my <- getsSession spointer
                    let onChoice (_, (PointUI cx cy, ButtonWidth font clen)) =
                          let blen | isSquareFont font = 2 * clen
                                   | otherwise = clen
                          in my == cy && mx >= cx && mx < cx + blen
                    case find onChoice kyxs of
                      Nothing | ikm `elem` keys ->
                        return (True, Left ikm, pointer)
                      Nothing -> if K.spaceKM `elem` keys
                                 then return (True, Left K.spaceKM, pointer)
                                 else ignoreKey
                      Just (ckm, _) -> case ckm of
                        Left km ->
                          if K.key km == K.Return && km `elem` keys
                          then return (True, Left km, pointer)
                          else interpretKey km
                        Right c  -> return (True, Right c, pointer)
                  K.RightButtonRelease ->
                    if | ikm `elem` keys -> return (True, Left ikm, pointer)
                       | K.escKM `elem` keys ->
                           return (True, Left K.escKM, pointer)
                       | otherwise -> ignoreKey
                  K.Space | firstItemOfNextPage <= maxIx ->
                    tmpResult firstItemOfNextPage
                  K.Char '?' | firstItemOfNextPage <= maxIx
                               && menuName == "help" ->  -- a hack
                    tmpResult firstItemOfNextPage
                  K.Unknown "SAFE_SPACE" ->
                    if firstItemOfNextPage <= maxIx
                    then tmpResult firstItemOfNextPage
                    else tmpResult clearIx
                  _ | ikm `elem` keys ->
                    return (True, Left ikm, pointer)
                  K.Up -> case findIndex xix $ reverse $ take ixOnPage kyxs of
                    Nothing -> interpretKey ikm{K.key=K.Left}
                    Just ix -> tmpResult (max 0 (pointer - ix - 1))
                  K.Left -> if pointer == 0 then tmpResult maxIx
                            else tmpResult (max 0 (pointer - 1))
                  K.Down -> case findIndex xix $ drop (ixOnPage + 1) kyxs of
                    Nothing -> interpretKey ikm{K.key=K.Right}
                    Just ix -> tmpResult (pointer + ix + 1)
                  K.Right -> if pointer == maxIx then tmpResult 0
                             else tmpResult (min maxIx (pointer + 1))
                  K.Home -> tmpResult clearIx
                  K.End -> tmpResult maxIx
                  _ | K.key ikm `elem` [K.PgUp, K.WheelNorth] ->
                    tmpResult (max 0 (pointer - ixOnPage - 1))
                  _ | K.key ikm `elem` [K.PgDn, K.WheelSouth] ->
                    -- This doesn't scroll by screenful when header very long
                    -- and menu non-empty, but that scenario is rare, so OK,
                    -- arrow keys may be used instead.
                    tmpResult (min maxIx firstItemOfNextPage)
                  K.Space -> if pointer == maxIx
                             then tmpResult clearIx
                             else tmpResult maxIx
                  _ -> error $ "unknown key" `showFailure` ikm
          pkm <- promptGetKey dm ovs sfBlank legalKeys
          interpretKey pkm
      m pointer okxRight =
        if null frs
        then return (True, Left K.escKM, pointer)
        else do
          (final, km, pointer1) <- page pointer okxRight
          let !_A1 = assert (either (`elem` keys) (const True) km) ()
          -- Pointer at a button included, hence greater than 0, not @clearIx@.
          let !_A2 = assert (0 <= pointer1 && pointer1 <= maxIx
                             `blame`  (pointer1, maxIx)) ()
          return (final, km, pointer1)
  return (maxIx, initIx, clearIx, m)

navigationKeys :: [K.KM]
navigationKeys = [ K.leftButtonReleaseKM, K.rightButtonReleaseKM
                 , K.returnKM, K.spaceKM
                 , K.upKM, K.leftKM, K.downKM, K.rightKM
                 , K.pgupKM, K.pgdnKM, K.wheelNorthKM, K.wheelSouthKM
                 , K.homeKM, K.endKM, K.controlP ]

-- | Find a position in a menu.
-- The arguments go from first menu line and menu page to the last,
-- in order. Their indexing is from 0. We select the nearest item
-- with the index equal or less to the pointer.
findKYX :: Int -> [OKX] -> Maybe (OKX, KYX, Int)
findKYX _ [] = Nothing
findKYX pointer (okx@(_, kyxs) : frs2) =
  case drop pointer kyxs of
    [] ->  -- not enough menu items on this page
      case findKYX (pointer - length kyxs) frs2 of
        Nothing ->  -- no more menu items in later pages
          case reverse kyxs of
            [] -> Nothing
            kyx : _ -> Just (okx, kyx, length kyxs - 1)
        res -> res
    kyx : _ -> Just (okx, kyx, pointer)

drawHighlight :: Int -> ButtonWidth -> Int -> AttrString -> AttrString
drawHighlight x1 (ButtonWidth font len) xstart as =
  let highableAttrs = [Color.defAttr, Color.defAttr {Color.fg = Color.BrBlack}]
      highAttr c | Color.acAttr c `notElem` highableAttrs
                   || Color.acChar c == ' ' = c
      highAttr c = c {Color.acAttr =
                        (Color.acAttr c) {Color.fg = Color.BrWhite}}
      cursorAttr c = c {Color.acAttr =
                          (Color.acAttr c)
                            {Color.bg = Color.HighlightNoneCursor}}
      -- This also highlights dull white item symbols, but who cares.
      lenUI = if isSquareFont font then len * 2 else len
      x1MinusXStartChars = if isSquareFont font
                           then (x1 - xstart) `div` 2
                           else x1 - xstart
      (as1, asRest) = splitAt x1MinusXStartChars as
      (as2, as3) = splitAt len asRest
      highW32 = Color.attrCharToW32
                . highAttr
                . Color.attrCharFromW32
      cursorW32 = Color.attrCharToW32
                  . cursorAttr
                  . Color.attrCharFromW32
      as2High = case map highW32 as2 of
        [] -> []
        ch : chrest -> cursorW32 ch : chrest
  in if x1 + lenUI < xstart
     then as
     else as1 ++ as2High ++ as3
