-- | A set of widgets for UI clients.
module Game.LambdaHack.Client.UI.WidgetClient
  ( drawOverlay, promptGetKey, promptGetInt
  , overlayToSlideshow, reportToSlideshow
  , displayMore, displayYesNo, getConfirms
  , displayChoiceScreen, displayChoiceLine
  , describeMainKeys
  , animate, fadeOutOrIn
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.DrawClient
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point

drawBaseFrame :: MonadClientUI m => ColorMode -> LevelId -> m SingleFrame
drawBaseFrame dm lid = do
  mleader <- getsClient _sleader
  tgtPos <- leaderTgtToPos
  xhairPos <- xhairToPos
  let anyPos = fromMaybe originPoint xhairPos
        -- if xhair invalid, e.g., on a wrong level; @draw@ ignores it later on
      pathFromLeader leader = Just <$> getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return ("------", Nothing)) targetDescLeader mleader
  sitemSel <- getsSession sitemSel
  xhairDesc <- targetDescXhair
  SessionUI{sselected, saimMode, smarkVision, smarkSmell, swaitTimes}
    <- getSession
  draw dm lid xhairPos tgtPos bfsmpath xhairDesc tgtDesc
       sselected saimMode sitemSel smarkVision smarkSmell swaitTimes

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m
            => ColorMode -> Bool -> Overlay -> LevelId -> m SingleFrame
drawOverlay dm sfBlank sfTop lid = do
  mbaseFrame <- if sfBlank
                then return Nothing
                else Just <$> drawBaseFrame dm lid
  return $! overlayFrame sfTop mbaseFrame
  -- TODO: here sfTop is possibly truncated wrt length

promptGetKey :: MonadClientUI m
             => ColorMode -> Overlay -> Bool -> [K.KM] -> m K.KM
promptGetKey dm ov sfBlank frontKeyKeys = do
  lid <- viewedLevel
  keyPressed <- anyKeyPressed
  lastPlayOld <- getsSession slastPlay
  km <- case lastPlayOld of
    km : kms | not keyPressed && km `K.elemOrNull` frontKeyKeys -> do
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
      let ov2 = ov <> if keyPressed then [toAttrLine "*interrupted*"] else mempty
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

promptGetInt :: MonadClientUI m => Overlay -> m K.KM
promptGetInt ov = do
  let frontKeyKeys = K.escKM : K.returnKM : K.backspaceKM
                     : map (K.KM K.NoModifier)
                         (map (K.Char . Char.intToDigit) [0..9])
  promptGetKey ColorFull ov False frontKeyKeys

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientUI m => Overlay -> m Slideshow
overlayToSlideshow ov = do
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  report <- getReport
  return $! splitOverlay lxsize (lysize + 1) report (ov, [])

reportToSlideshow :: MonadClientUI m => m Slideshow
reportToSlideshow = overlayToSlideshow mempty

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

-- | Display a slideshow, awaiting confirmation for each slide
-- and returning a boolean.
getConfirms :: MonadClientUI m
            => ColorMode -> [K.KM] -> [K.KM] -> Slideshow -> m Bool
getConfirms dm trueKeys falseKeys slides = do
  if slides == mempty then return False else do
    (ekm, _) <- displayChoiceScreen dm False 0 slides (trueKeys ++ falseKeys)
    case ekm of
      Left km -> return $! km `K.elemOrNull` trueKeys
      Right slot -> assert `failure` slot

-- | Add a prompt to report and wait for a player keypress.
displayConfirm :: MonadClientUI m
               => ColorMode -> [K.KM] -> [K.KM] -> Text -> m Bool
displayConfirm dm trueKeys falseKeys prompt = do
  promptAdd prompt
  -- Two frames drawn total (unless @prompt@ very long).
  slides <- reportToSlideshow
  b <- getConfirms dm trueKeys falseKeys slides
  return b

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

-- | Print a prompt and an overlay and wait for a player keypress.
displayChoiceLine :: MonadClientUI m => Overlay -> [K.KM] -> m K.KM
displayChoiceLine ov extraKeys = do
  slides <- overlayToSlideshow ov
  (ekm, _) <- displayChoiceScreen ColorFull False 0 slides extraKeys
  return $! either id (assert `failure` ekm) ekm

describeMainKeys :: MonadClientUI m => m Text
describeMainKeys = do
  saimMode <- getsSession saimMode
  Binding{brevMap} <- getsSession sbinding
  Config{configVi, configLaptop} <- getsSession sconfig
  xhair <- getsClient sxhair
  let kmEscape = head $
        M.findWithDefault [K.KM K.NoModifier K.Esc]
                          ByAimMode {notAiming = MainMenu, aiming = Cancel}
                          brevMap
      kmReturn = head $
        M.findWithDefault [K.KM K.NoModifier K.Return]
                          ByAimMode{notAiming = Help $ Just "", aiming = Accept}
                          brevMap
      (mkp, moveKeys) | configVi = ("keypad or", "hjklyubn,")
                      | configLaptop = ("keypad or", "uk8o79jl,")
                      | otherwise = ("", "keypad keys,")
      tgtKind = case xhair of
        TEnemy _ True -> "at actor"
        TEnemy _ False -> "at enemy"
        TEnemyPos _ _ _ True -> "at actor"
        TEnemyPos _ _ _ False -> "at enemy"
        TPoint{} -> "at position"
        TVector{} -> "with a vector"
      keys | isNothing saimMode =
        "Explore with" <+> mkp <+> "keys or mouse: ["
        <> moveKeys
        <+> T.intercalate ", "
             (map K.showKM [kmReturn, kmEscape])
        <> "]"
           | otherwise =
        "Aim" <+> tgtKind <+> "with" <+> mkp <+> "keys or mouse: ["
        <> moveKeys
        <+> T.intercalate ", "
             (map K.showKM [kmReturn, kmEscape])
        <> "]"
  return $! keys

-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m Frames
animate arena anim = do
  report <- getReport
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
