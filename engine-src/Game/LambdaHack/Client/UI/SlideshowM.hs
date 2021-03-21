-- | Monadic operations on slideshows and related data.
module Game.LambdaHack.Client.UI.SlideshowM
  ( overlayToSlideshow, reportToSlideshow, reportToSlideshowKeep
  , displaySpaceEsc, displayMore, displayMoreKeep, displayYesNo, getConfirms
  , displayChoiceScreen
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.ItemSlot
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
  CCUI{coscreen=ScreenContent{rwidth, rwrap}} <- getsSession sccui
  UIOptions{uScreen1PerLine} <- getsSession sUIOptions
  report <- getReportUI
  recordHistory  -- report will be shown soon, remove it to history
  fontSetup <- getFontSetup
  return $! splitOverlay fontSetup uScreen1PerLine rwidth y rwrap
                         report keys okx

-- | Split current report into a slideshow.
reportToSlideshow :: MonadClientUI m => [K.KM] -> m Slideshow
reportToSlideshow keys = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  overlayToSlideshow (rheight - 2) keys (EM.empty, [])

-- | Split current report into a slideshow. Keep report unchanged.
reportToSlideshowKeep :: MonadClientUI m => [K.KM] -> m Slideshow
reportToSlideshowKeep keys = do
  CCUI{coscreen=ScreenContent{rwidth, rheight, rwrap}} <- getsSession sccui
  UIOptions{uScreen1PerLine} <- getsSession sUIOptions
  report <- getReportUI
  -- Don't do @recordHistory@; the message is important, but related
  -- to the messages that come after, so should be shown together.
  fontSetup <- getFontSetup
  return $! splitOverlay fontSetup uScreen1PerLine rwidth (rheight - 2) rwrap
                         report keys (EM.empty, [])

-- | Display a message. Return value indicates if the player wants to continue.
-- Feature: if many pages, only the last SPACE exits (but first ESC).
displaySpaceEsc :: (MonadClient m, MonadClientUI m)
                => ColorMode -> Text -> m Bool
displaySpaceEsc dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  -- Two frames drawn total (unless @prompt@ very long).
  slides <- reportToSlideshow [K.spaceKM, K.escKM]
  km <- getConfirms dm [K.spaceKM, K.escKM] slides
  return $! km == K.spaceKM

-- | Display a message. Ignore keypresses.
-- Feature: if many pages, only the last SPACE exits (but first ESC).
displayMore :: (MonadClient m, MonadClientUI m) => ColorMode -> Text -> m ()
displayMore dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  slides <- reportToSlideshow [K.spaceKM]
  void $ getConfirms dm [K.spaceKM, K.escKM] slides

displayMoreKeep :: (MonadClient m, MonadClientUI m) => ColorMode -> Text -> m ()
displayMoreKeep dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  slides <- reportToSlideshowKeep [K.spaceKM]
  void $ getConfirms dm [K.spaceKM, K.escKM] slides

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: (MonadClient m, MonadClientUI m) => ColorMode -> Text -> m Bool
displayYesNo dm prompt = do
  unless (T.null prompt) $ msgLnAdd MsgPromptGeneric prompt
  let yn = map K.mkChar ['y', 'n']
  slides <- reportToSlideshow yn
  km <- getConfirms dm (K.escKM : yn) slides
  return $! km == K.mkChar 'y'

getConfirms :: (MonadClient m, MonadClientUI m)
            => ColorMode -> [K.KM] -> Slideshow -> m K.KM
getConfirms dm extraKeys slides = do
  ekm <- displayChoiceScreen "" dm False slides extraKeys
  return $! either id (error $ "" `showFailure` ekm) ekm

-- | Display a, potentially, multi-screen menu and return the chosen
-- key or item slot label (and the index in the whole menu so that the cursor
-- can again be placed at that spot next time menu is displayed).
--
-- This function is the only source of menus and so, effectively, UI modes.
displayChoiceScreen :: forall m . (MonadClient m, MonadClientUI m)
                    => String -> ColorMode -> Bool -> Slideshow -> [K.KM]
                    -> m (Either K.KM SlotChar)
displayChoiceScreen menuName dm sfBlank frsX extraKeys = do
  let frs = slideshow frsX
      keys = concatMap (concatMap (fromLeft [] . fst) . snd) frs
             ++ extraKeys
      !_A = assert (K.escKM `elem` extraKeys) ()
      navigationKeys = [ K.leftButtonReleaseKM, K.rightButtonReleaseKM
                       , K.returnKM, K.spaceKM
                       , K.upKM, K.leftKM, K.downKM, K.rightKM
                       , K.pgupKM, K.pgdnKM, K.wheelNorthKM, K.wheelSouthKM
                       , K.homeKM, K.endKM, K.controlP ]
      legalKeys = keys ++ navigationKeys
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
      maxIx = length (concatMap snd frs) - 1
      allOKX = concatMap snd frs
      initIx = case findIndex (isRight . fst) allOKX of
        Just p -> p
        _ -> 0  -- can't be @length allOKX@ or a multi-page item menu
                -- mangles saved index of other item munus
      clearIx = if initIx > maxIx then 0 else initIx
      page :: Int -> m (Either K.KM SlotChar, Int)
      page pointer = assert (pointer >= 0) $ case findKYX pointer frs of
        Nothing -> error $ "no menu keys" `showFailure` frs
        Just ( (ovs, kyxs)
             , (ekm, (PointUI x1 y, ButtonWidth fontX1 len))
             , ixOnPage ) -> do
          let highableAttrs =
                [Color.defAttr, Color.defAttr {Color.fg = Color.BrBlack}]
              highAttr x | Color.acAttr x `notElem` highableAttrs
                           || Color.acChar x == ' ' = x
              highAttr x = x {Color.acAttr =
                                (Color.acAttr x) {Color.fg = Color.BrWhite}}
              cursorAttr x = x {Color.acAttr =
                                  (Color.acAttr x)
                                    {Color.bg = Color.HighlightNoneCursor}}
              -- This also highlights dull white item symbols, but who cares.
              lenUI = if isSquareFont fontX1 then len * 2 else len
              drawHighlight xstart xs | x1 + lenUI < xstart = xs
                                      | otherwise =
                let x1MinusXStartChars = if isSquareFont fontX1
                                         then (x1 - xstart) `div` 2
                                         else x1 - xstart
                    (xs1, xsRest) = splitAt x1MinusXStartChars xs
                    (xs2, xs3) = splitAt len xsRest
                    highW32 = Color.attrCharToW32
                              . highAttr
                              . Color.attrCharFromW32
                    cursorW32 = Color.attrCharToW32
                                . cursorAttr
                                . Color.attrCharFromW32
                    xs2High = case map highW32 xs2 of
                      [] -> []
                      xh : xhrest -> cursorW32 xh : xhrest
                in xs1 ++ xs2High ++ xs3
              ovs1 = EM.map (updateLine y drawHighlight) ovs
              ignoreKey = page pointer
              pageLen = length kyxs
              xix :: KYX -> Bool
              xix (_, (PointUI x1' _, _)) = x1' <= x1 + 2 && x1' >= x1 - 2
              firstRowOfNextPage = pointer + pageLen - ixOnPage
              restOKX = drop firstRowOfNextPage allOKX
              firstItemOfNextPage = case findIndex (isRight . fst) restOKX of
                Just p -> p + firstRowOfNextPage
                _ -> firstRowOfNextPage
              interpretKey :: K.KM -> m (Either K.KM SlotChar, Int)
              interpretKey ikm =
                case K.key ikm of
                  _ | ikm == K.controlP -> do
                    -- Silent, because any prompt would be shown too late.
                    printScreen
                    ignoreKey
                  K.Return -> case ekm of
                    Left (km : _) ->
                      if K.key km == K.Return && km `elem` keys
                      then return (Left km, pointer)
                      else interpretKey km
                    Left [] -> error $ "" `showFailure` ikm
                    Right c -> return (Right c, pointer)
                  K.LeftButtonRelease -> do
                    PointUI mx my <- getsSession spointer
                    let onChoice (_, (PointUI cx cy, ButtonWidth font clen)) =
                          let blen | isSquareFont font = 2 * clen
                                   | otherwise = clen
                          in my == cy && mx >= cx && mx < cx + blen
                    case find onChoice kyxs of
                      Nothing | ikm `elem` keys -> return (Left ikm, pointer)
                      Nothing -> if K.spaceKM `elem` keys
                                 then return (Left K.spaceKM, pointer)
                                 else ignoreKey
                      Just (ckm, _) -> case ckm of
                        Left (km : _) ->
                          if K.key km == K.Return && km `elem` keys
                          then return (Left km, pointer)
                          else interpretKey km
                        Left [] -> error $ "" `showFailure` ikm
                        Right c  -> return (Right c, pointer)
                  K.RightButtonRelease ->
                    if | ikm `elem` keys -> return (Left ikm, pointer)
                       | K.escKM `elem` keys -> return (Left K.escKM, pointer)
                       | otherwise -> ignoreKey
                  K.Space | firstItemOfNextPage <= maxIx ->
                    page firstItemOfNextPage
                  K.Unknown "SAFE_SPACE" ->
                    if firstItemOfNextPage <= maxIx
                    then page firstItemOfNextPage
                    else page clearIx
                  _ | ikm `elem` keys ->
                    return (Left ikm, pointer)
                  K.Up -> case findIndex xix $ reverse $ take ixOnPage kyxs of
                    Nothing -> interpretKey ikm{K.key=K.Left}
                    Just ix -> page (max 0 (pointer - ix - 1))
                  K.Left -> if pointer == 0 then page maxIx
                            else page (max 0 (pointer - 1))
                  K.Down -> case findIndex xix $ drop (ixOnPage + 1) kyxs of
                    Nothing -> interpretKey ikm{K.key=K.Right}
                    Just ix -> page (pointer + ix + 1)
                  K.Right -> if pointer == maxIx then page 0
                             else page (min maxIx (pointer + 1))
                  K.Home -> page clearIx
                  K.End -> page maxIx
                  _ | K.key ikm `elem` [K.PgUp, K.WheelNorth] ->
                    page (max 0 (pointer - ixOnPage - 1))
                  _ | K.key ikm `elem` [K.PgDn, K.WheelSouth] ->
                    -- This doesn't scroll by screenful when header very long
                    -- and menu non-empty, but that scenario is rare, so OK,
                    -- arrow keys may be used instead.
                    page (min maxIx firstItemOfNextPage)
                  K.Space -> if pointer == maxIx
                             then page clearIx
                             else page maxIx
                  _ -> error $ "unknown key" `showFailure` ikm
          keyPressed <- anyKeyPressed
          pkm <- promptGetKey keyPressed dm ovs1 sfBlank legalKeys
          interpretKey pkm
  menuIxMap <- getsSession smenuIxMap
  -- Beware, values in @menuIxMap@ may be negative (meaning: a key, not slot).
  let menuIx | menuName == "" = clearIx
             | otherwise =
               maybe clearIx (+ initIx) (M.lookup menuName menuIxMap)
                 -- this may be negative, from different context
  (km, pointer) <- if null frs
                   then return (Left K.escKM, menuIx)
                   else page $ max clearIx $ min maxIx menuIx
                          -- the saved index could be from different context
  unless (menuName == "") $
    modifySession $ \sess ->
      sess {smenuIxMap = M.insert menuName (pointer - initIx) menuIxMap}
  assert (either (`elem` keys) (const True) km) $ return km
