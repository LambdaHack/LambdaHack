-- | A set of Slideshow monad operations.
module Game.LambdaHack.Client.UI.SlideshowM
  ( overlayToSlideshow, reportToSlideshow
  , displayMore, displayYesNo, getConfirms
  , displayChoiceScreen, pickNumber
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char

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
overlayToSlideshow :: MonadClientUI m => Y -> OKX -> m Slideshow
overlayToSlideshow y okx = do
  arena <- getArenaUI
  Level{lxsize} <- getLevel arena  -- TODO: screen length or viewLevel
  report <- getReportUI
  return $! splitOverlay lxsize y report okx

-- | Split current report into a slideshow.
reportToSlideshow :: MonadClientUI m => m Slideshow
reportToSlideshow = do
  arena <- getArenaUI
  Level{lysize} <- getLevel arena
  overlayToSlideshow (lysize + 1) ([], [])

-- | Display a message with a @more@ prompt.
-- Return value indicates if the player tried to cancel/escape.
displayMore :: MonadClientUI m => ColorMode -> Text -> m Bool
displayMore dm prompt =
  displayConfirm dm [K.spaceKM] [K.escKM] (prompt <+> tmoreMsg)

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Text -> m Bool
displayYesNo dm prompt =
  displayConfirm dm [K.KM K.NoModifier (K.Char 'y')]
                    [K.KM K.NoModifier (K.Char 'n'), K.escKM]
                    (prompt <+> tyesnoMsg)

-- | Add a prompt to report and wait for a player keypress.
displayConfirm :: MonadClientUI m
               => ColorMode -> [K.KM] -> [K.KM] -> Text -> m Bool
displayConfirm dm trueKeys falseKeys prompt = do
  promptAdd prompt
  -- Two frames drawn total (unless @prompt@ very long).
  slides <- reportToSlideshow
  km <- getConfirms dm (trueKeys ++ falseKeys) slides
  return $! km `K.elemOrNull` trueKeys

getConfirms :: MonadClientUI m
            => ColorMode -> [K.KM] -> Slideshow -> m K.KM
getConfirms dm extraKeys slides = do
  (ekm, _) <- displayChoiceScreen dm False 0 slides extraKeys
  return $! either id (assert `failure` ekm) ekm

displayChoiceScreen :: forall m . MonadClientUI m
                    => ColorMode -> Bool -> Int -> Slideshow -> [K.KM]
                    -> m (Either K.KM SlotChar, Int)
displayChoiceScreen dm sfBlank pointer0 frsX extraKeys = do
  -- We don't create keys from slots, so they have to be @in extraKeys@.
  let frs = slideshow frsX
      keys = concatMap (mapMaybe (either Just (const Nothing) . fst) . snd) frs
             ++ extraKeys
      scrollKeys = [K.leftButtonReleaseKM, K.returnKM, K.upKM, K.downKM]
      pageKeys = [ K.spaceKM, K.pgupKM, K.pgdnKM, K.wheelNorthKM, K.wheelSouthKM
                 , K.homeKM, K.endKM ]
      legalKeys = keys ++ scrollKeys ++ pageKeys
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
                      Nothing -> ignoreKey
                      Just (ckm, _) -> case ckm of
                        Left km -> interpretKey km
                        _ -> return (ckm, pointer)
                  K.Up | pointer == 0 -> page maxIx
                  K.Up -> page (max 0 (pointer - 1))
                  K.Down | pointer == maxIx -> page 0
                  K.Down -> page (min maxIx (pointer + 1))
                  K.Home -> page 0
                  K.End -> page maxIx
                  _ | K.key ikm `elem` [K.PgUp, K.WheelNorth] ->
                    page (max 0 (pointer - ixOnPage - 1))
                  _ | K.key ikm `elem` [K.PgDn, K.WheelSouth] ->
                    page (min maxIx (pointer + pageLen - ixOnPage))
                  K.Space | pointer == maxIx && K.spaceKM `elem` extraKeys ->
                    -- If Space permitted, only exits at the end of slideshow.
                    return (Left K.spaceKM, pointer)
                  K.Space ->
                    page (min maxIx (pointer + pageLen - ixOnPage))
                  _ | ikm `K.elemOrNull` keys ->
                    return (Left ikm, pointer)
                  _ -> assert `failure` "unknown key" `twith` ikm
          pkm <- promptGetKey dm ov1 sfBlank legalKeys
          interpretKey pkm
  page pointer0

promptGetInt :: MonadClientUI m => Overlay -> m K.KM
promptGetInt ov = do
  let frontKeyKeys = K.escKM : K.returnKM : K.backspaceKM
                     : map (K.KM K.NoModifier)
                         (map (K.Char . Char.intToDigit) [0..9])
  promptGetKey ColorFull ov False frontKeyKeys

pickNumber :: MonadClientUI m => Bool -> Int -> m (Either Text Int)
pickNumber askNumber kAll = do
  let gatherNumber kDefaultRaw = do
        let kDefault = min kAll kDefaultRaw
            kprompt = "Choose number [digits, BACKSPACE, RET("
                      <> tshow kDefault
                      <> "), ESC]"
        promptAdd kprompt
        (al, _) : _ <- slideshow <$> reportToSlideshow
        kkm <- promptGetInt al
        case K.key kkm of
          K.Char l | kDefault == kAll -> gatherNumber $ Char.digitToInt l
          K.Char l -> gatherNumber $ kDefault * 10 + Char.digitToInt l
          K.BackSpace -> gatherNumber $ kDefault `div` 10
          K.Return -> return $ Right kDefault
          K.Esc -> return $ Left "never mind"
          _ -> assert `failure` "unexpected key:" `twith` kkm
  if | kAll == 0 -> return $ Left "no number of items can be chosen"
     | kAll == 1 || not askNumber -> return $ Right kAll
     | otherwise -> do
         num <- gatherNumber kAll
         case num of
           Right 0 -> return $ Left "zero items chosen"
           _ -> return num
