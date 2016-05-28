-- | A set of Slideshow monad operations.
module Game.LambdaHack.Client.UI.SlideshowM
  ( overlayToSlideshow, reportToSlideshow
  , displaySpaceEsc, displayMore, displayYesNo, getConfirms
  , displayChoiceScreen
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point

-- | Add current report to the overlay, split the result and produce,
-- possibly, many slides.
overlayToSlideshow :: MonadClientUI m => Y -> [K.KM] -> OKX -> m Slideshow
overlayToSlideshow y keys okx = do
  arena <- getArenaUI
  Level{lxsize} <- getLevel arena  -- TODO: screen length or viewLevel
  report <- getReportUI
  return $! splitOverlay lxsize y report keys okx

-- | Split current report into a slideshow.
reportToSlideshow :: MonadClientUI m => [K.KM] -> m Slideshow
reportToSlideshow keys = do
  arena <- getArenaUI
  Level{lysize} <- getLevel arena
  overlayToSlideshow (lysize + 1) keys ([], [])

-- | Display a message. Return value indicates if the player tried to cancel.
-- Feature: if many pages, only the last SPACE exits (but first ESC).
displaySpaceEsc :: MonadClientUI m => ColorMode -> Text -> m Bool
displaySpaceEsc dm prompt = do
  promptAdd prompt
  -- Two frames drawn total (unless @prompt@ very long).
  slides <- reportToSlideshow [K.spaceKM, K.escKM]
  km <- getConfirms dm [K.spaceKM, K.escKM] slides
  return $! km == K.spaceKM

-- | Display a message. Ignore keypresses.
-- Feature: if many pages, only the last SPACE exits (but first ESC).
displayMore :: MonadClientUI m => ColorMode -> Text -> m ()
displayMore dm prompt = do
  promptAdd prompt
  slides <- reportToSlideshow [K.spaceKM]
  void $ getConfirms dm [K.spaceKM, K.escKM] slides

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Text -> m Bool
displayYesNo dm prompt = do
  promptAdd prompt
  let yn = map (K.KM K.NoModifier . K.Char) ['y', 'n']
  slides <- reportToSlideshow yn
  km <- getConfirms dm (K.escKM : yn) slides
  return $! km == K.KM K.NoModifier (K.Char 'y')

getConfirms :: MonadClientUI m
            => ColorMode -> [K.KM] -> Slideshow -> m K.KM
getConfirms dm extraKeys slides = do
  (ekm, _) <- displayChoiceScreen dm False 0 slides extraKeys
  return $! either id (assert `failure` ekm) ekm

displayChoiceScreen :: forall m . MonadClientUI m
                    => ColorMode -> Bool -> Int -> Slideshow -> [K.KM]
                    -> m (Either K.KM SlotChar, Int)
displayChoiceScreen dm sfBlank pointer0 frsX extraKeys = do
  let frs = slideshow frsX
      keys = concatMap (mapMaybe (either Just (const Nothing) . fst) . snd) frs
             ++ extraKeys
      !_A = assert (K.escKM `elem` extraKeys) ()
      navigationKeys = [ K.leftButtonReleaseKM, K.returnKM, K.spaceKM
                       , K.upKM, K.leftKM, K.downKM, K.rightKM
                       , K.pgupKM, K.pgdnKM, K.wheelNorthKM, K.wheelSouthKM
                       , K.homeKM, K.endKM ]
      legalKeys = keys ++ navigationKeys
      -- The arguments go from first menu line and menu page to the last,
      -- in order. Their indexing is from 0. We select the closest item
      -- with the index equal or less to the pointer, or if there is none,
      -- with a larger index.
      findKYX :: Int -> [OKX] -> Maybe (OKX, KYX, Int)
      findKYX _ [] = Nothing
      findKYX pointer (okx@(_, kyxs) : frs2) =
        case drop pointer kyxs of
          [] ->  -- not enough menu items on this page
            case findKYX (pointer - length kyxs) frs2 of
              Nothing ->  -- no more menu items in later pages
                case reverse kyxs of
                  [] -> Nothing
                  kyx : _ -> Just (okx, kyx, length kyxs)
              okyx -> okyx
          kyx : _ -> Just (okx, kyx, pointer)
      maxIx = length (concatMap snd frs) - 1
      page :: Int -> m (Either K.KM SlotChar, Int)
      page pointer = case findKYX pointer frs of
        Nothing -> assert `failure` "no menu keys" `twith` frs
        Just ((ov, kyxs), (ekm, (y, x1, x2)), ixOnPage) -> do
          let greyBG x | Color.acAttr x /= Color.defAttr = x
              greyBG x = x{Color.acAttr =
                            (Color.acAttr x){Color.fg = Color.BrWhite}}
              drawHighlight xs =
                let (xs1, xsRest) = splitAt x1 xs
                    (xs2, xs3) = splitAt (x2 - x1) xsRest
                in xs1 ++ map greyBG xs2 ++ xs3
              ov1 = updateOverlayLine y drawHighlight ov
              ignoreKey = page pointer
              pageLen = length kyxs
              interpretKey :: K.KM -> m (Either K.KM SlotChar, Int)
              interpretKey ikm =
                case K.key ikm of
                  K.Return | ekm /= Left K.returnKM -> case ekm of
                    Left km -> interpretKey km
                    _ -> return (ekm, pointer)
                  K.LeftButtonRelease -> do
                    Point{..} <- getsSession spointer
                    let onChoice (_, (cy, cx1, cx2)) =
                          cy == py && cx1 <= px && cx2 > px
                    case find onChoice kyxs of
                      Nothing | ikm `elem` keys -> return (Left ikm, pointer)
                      Nothing -> ignoreKey
                      Just (ckm, _) -> case ckm of
                        Left km -> interpretKey km
                        _ -> return (ckm, pointer)
                  K.Space | pointer + pageLen - ixOnPage <= maxIx ->
                    page (pointer + pageLen - ixOnPage)
                  K.Unknown "Space" | pointer + pageLen - ixOnPage <= maxIx ->
                    page (pointer + pageLen - ixOnPage)
                  _ | ikm `elem` keys ->
                    return (Left ikm, pointer)
                  _ | K.key ikm `elem` [K.Up, K.Left] ->
                    if pointer == 0 then page maxIx
                    else page (max 0 (pointer - 1))
                  _ | K.key ikm `elem` [K.Down, K.Right] ->
                    if pointer == maxIx then page 0
                    else page (min maxIx (pointer + 1))
                  K.Home -> page 0
                  K.End -> page maxIx
                  _ | K.key ikm `elem` [K.PgUp, K.WheelNorth] ->
                    page (max 0 (pointer - ixOnPage - 1))
                  _ | K.key ikm `elem` [K.PgDn, K.WheelSouth] ->
                    page (min maxIx (pointer + pageLen - ixOnPage))
                  K.Space -> ignoreKey
                  K.Unknown "Space" -> ignoreKey
                  _ -> assert `failure` "unknown key" `twith` ikm
          pkm <- promptGetKey dm ov1 sfBlank legalKeys
          interpretKey pkm
  (km, pointer) <- if null frs
                   then return (Left K.escKM, pointer0)
                   else page pointer0
  assert (either (`elem` keys) (const True) km) $ return (km, pointer)
