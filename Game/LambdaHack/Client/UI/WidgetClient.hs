-- | A set of widgets for UI clients.
module Game.LambdaHack.Client.UI.WidgetClient
  ( displayMore, displayYesNo, displayChoiceScreen, displayChoiceLine
  , describeMainKeys
  , promptToSlideshow, overlayToSlideshow
  , animate, fadeOutOrIn
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T

import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient hiding (liftIO)
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.DrawClient
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State

-- | Display a message with a @-more-@ prompt.
-- Return value indicates if the player tried to cancel/escape.
displayMore :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayMore dm prompt = do
  slides <- promptToSlideshow $ prompt <+> moreMsg
  -- Two frames drawn total (unless @prompt@ very long).
  getConfirms dm [K.spaceKM] [K.escKM] slides

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayYesNo dm prompt = do
  slides <- promptToSlideshow $ prompt <+> yesnoMsg
  -- Two frames drawn total (unless @prompt@ very long).
  getConfirms dm [K.KM K.NoModifier (K.Char 'y')]
                 [K.KM K.NoModifier (K.Char 'n'), K.escKM] slides

displayChoiceScreen :: forall m . MonadClientUI m
                    => Bool -> Int -> [OKX] -> [K.KM]
                    -> m (Either K.KM SlotChar, Int)
displayChoiceScreen sfBlank pointer0 frs extraKeys = do
  -- We don't create keys from slots, so they have to be @in extraKeys@.
  let keys = concatMap (mapMaybe (keyOfEKM (-1) . fst) . snd) frs
             ++ extraKeys
      scrollKeys = [K.leftButtonKM, K.returnKM, K.upKM, K.downKM]
      pageKeys = [K.spaceKM, K.pgupKM, K.pgdnKM, K.homeKM, K.endKM]
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
                  K.LeftButtonPress -> do
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
                  K.PgUp ->
                    page (max 0 (pointer - ixOnPage - 1))
                  K.PgDn ->
                    page (min maxIx (pointer + pageLen - ixOnPage))
                  K.Space | pointer == maxIx && K.spaceKM `elem` extraKeys ->
                    -- If Space permitted, only exits at the end of slideshow.
                    return (Left K.spaceKM, pointer)
                  K.Space ->
                    page (min maxIx (pointer + pageLen - ixOnPage))
                  _ | ikm `K.elemOrNull` keys ->
                    return (Left ikm, pointer)
                  _ -> assert `failure` "unknown key" `twith` ikm
          pkm <- promptGetKey ov1 sfBlank legalKeys
          interpretKey pkm
  page pointer0

-- TODO: generalize displayChoiceLine and getInitConfirms to a single op?
--       but don't enable SPACE, etc. if only one screen (or only prompt)
--       don't truncate then, but
-- If many overlays, scroll screenfuls with SPACE, etc.
-- | Print a prompt and an overlay and wait for a player keypress.
displayChoiceLine :: MonadClientUI m => Msg -> Overlay -> [K.KM] -> m K.KM
displayChoiceLine prompt ov extraKeys = do
  slides <- overlayToSlideshow prompt ov
  getConfirmsKey ColorFull extraKeys slides

describeMainKeys :: MonadClientUI m => m Msg
describeMainKeys = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let underAI = isAIFact fact
  stgtMode <- getsSession stgtMode
  Binding{brevMap} <- askBinding
  Config{configVi, configLaptop} <- askConfig
  cursor <- getsClient scursor
  let kmLeftButtonPress = head $
        M.findWithDefault [K.KM K.NoModifier K.LeftButtonPress]
                          defaultCmdLMB brevMap
      kmEscape = head $
        M.findWithDefault [K.KM K.NoModifier K.Esc] (ByMode MainMenu Cancel) brevMap
      kmReturn = head $
        M.findWithDefault [K.KM K.NoModifier K.Return] (ByMode (Help $ Just "") Accept) brevMap
      moveKeys | configVi = "hjklyubn, "
               | configLaptop = "uk8o79jl, "
               | otherwise = ""
      tgtKind = case cursor of
        TEnemy _ True -> "at actor"
        TEnemy _ False -> "at enemy"
        TEnemyPos _ _ _ True -> "at actor"
        TEnemyPos _ _ _ False -> "at enemy"
        TPoint{} -> "at position"
        TVector{} -> "with a vector"
      keys | underAI = ""
           | isNothing stgtMode =
        "Explore with keypad or keys or mouse: ["
        <> moveKeys
        <> T.intercalate ", "
             (map K.showKM [kmLeftButtonPress, kmEscape])
        <> "]"
           | otherwise =
        "Aim" <+> tgtKind <+> "with keypad or keys or mouse: ["
        <> moveKeys
        <> T.intercalate ", "
             (map K.showKM [kmLeftButtonPress, kmReturn, kmEscape])
        <> "]"
  report <- getsSession sreport
  return $! if nullReport report then keys else ""

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadClientUI m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt mempty

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  promptAI <- msgPromptAI
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  sreport <- getsSession sreport
  let msg = splitReport lxsize (prependMsg promptAI (addMsg sreport prompt))
  return $! splitOverlay (lysize + 1) msg overlay

-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m Frames
animate arena anim = do
  sreport <- getsSession sreport
  promptAI <- msgPromptAI
  let over = renderReport (prependMsg promptAI sreport)
      topLineOnly = truncateToOverlay over
  baseFrame <- drawBaseFrame ColorFull arena
  let basicFrame = overlayFrame topLineOnly $ Just baseFrame
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
