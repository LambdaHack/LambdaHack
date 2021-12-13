{-# LANGUAGE RankNTypes #-}
-- | Verifying, aggregating and displaying binding of keys to commands.
module Game.LambdaHack.Client.UI.KeyBindings
  ( keyHelp, okxsN
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.Slideshow
import qualified Game.LambdaHack.Definition.Color as Color

-- | Produce a set of help/menu screens from the key bindings.
--
-- When the intro screen mentions KP_5, this really is KP_Begin,
-- but since that is harder to understand we assume a different, non-default
-- state of NumLock in the help text than in the code that handles keys.
keyHelp :: CCUI -> FontSetup -> [(Text, OKX)]
keyHelp CCUI{ coinput=coinput@InputContent{..}
            , coscreen=ScreenContent{rwidth, rheight} } FontSetup{..} =
  let
    movBlurb1 =
      [ "Walk throughout a level with mouse or numeric keypad (right diagram below)"
      , "or the Vi editor keys (middle) or the left-hand movement keys (left). Run until"
      , "disturbed with Shift or Control. Go-to a position with LMB (left mouse button)."
      , "In aiming mode, the same keys (and mouse) move the aiming crosshair."
      ]
    movSchema =
      [ "     q w e     y k u     7 8 9"
      , "      \\|/       \\|/       \\|/"
      , "     a-s-d     h-.-l     4-5-6"
      , "      /|\\       /|\\       /|\\"
      , "     z x c     b j n     1 2 3"
      ]
    movBlurb2 =
      [ "Press `KP_5` (`5` on keypad) to wait, bracing for impact, which reduces any"
      , "damage taken and prevents displacement by foes. Press `S-KP_5` or `C-KP_5`"
      , "(the same key with Shift or Control) to lurk 0.1 of a turn, without bracing."
      , ""
      , "Displace enemies by running into them with Shift/Control or S-LMB. Search,"
      , "open, descend and melee by bumping into walls, doors, stairs and enemies."
      , "The best, and not on cooldown, melee weapon is automatically chosen"
      , "for attack from your equipment and from among your body parts."
      ]
    minimalBlurb =
      [ "The following few commands, joined with the movement and running keys,"
      , "let you accomplish almost anything in the game, though not necessarily"
      , "with the fewest keystrokes. You can also play the game exclusively"
      , "with a mouse, or both mouse and keyboard (e.g., mouse for go-to"
      , "and terrain inspection and keyboard for everything else). Lastly,"
      , "you can select a command with arrows or mouse directly from the help"
      , "screen or the dashboard and execute it on the spot."
      ]
    itemAllEnding =
      [ "Note how lower case item commands (stash item, equip item) place items"
      , "into a particular item store, while upper case item commands (manage Inventory,"
      , "manage Outfit) open management menu for a store. Once a store menu is opened,"
      , "you can switch stores with `<` and `>`, so the multiple commands only determine"
      , "the starting item store. Each store is accessible from the dashboard as well."
      ]
    mouseBasicsBlurb =
      [ "Screen area and UI mode (exploration/aiming) determine mouse click"
      , "effects. Here we give an overview of effects of each button over"
      , "the game map area. The list includes not only left and right buttons,"
      , "but also the optional middle mouse button (MMB) and the mouse wheel,"
      , "which is also used over menus to move selection. For mice without RMB,"
      , "one can use Control key with LMB and for mice without MMB, one can use"
      , "C-RMB or C-S-LMB."
      ]
    mouseAreasBlurb =
      [ "Next we show mouse button effects per screen area, in exploration and"
      , "(if different) aiming mode. Note that mouse is optional. Keyboard suffices,"
      , "occasionally requiring a lookup for an obscure command key in help screens."
      ]
    mouseAreasMini =
      [ "Mouse button effects per screen area, in exploration and in aiming modes"
      ]
    movTextEnd = "Press SPACE or PGDN to advance or ESC to see the map again."
    lastHelpEnd = "Use PGUP to go back and ESC to see the map again."
    seeAlso = "For more playing instructions see file PLAYING.md."
    offsetCol2 = 12
    pickLeaderDescription =
      [ fmt offsetCol2 "0, 1 ... 9"
                       "pick a particular actor as the new pointman"
      ]
    casualDescription = "Minimal cheat sheet for casual play"
    fmt0 n k h = T.justifyLeft n ' ' k <> " " <> h
    fmt n k h = " " <> fmt0 n k h
    keyCaption = fmt offsetCol2 "keys" "command"
    mouseOverviewCaption = fmt offsetCol2 "keys" "command (exploration/aiming)"
    spLen = textSize monoFont " "
    okxs cat headers footers = xytranslateOKX spLen 0 $
      okxsN coinput monoFont propFont offsetCol2 (const False)
            True cat headers footers
    mergeOKX :: OKX -> OKX -> OKX
    mergeOKX okx1 okx2 =
      let off = 1 + maxYofFontOverlayMap (fst okx1)
      in sideBySideOKX 0 off okx1 okx2
    catLength cat = length $ filter (\(_, (cats, desc, _)) ->
      cat `elem` cats && (desc /= "" || CmdInternal `elem` cats)) bcmdList
    keyM = 13
    keyB = 31
    truncatem b = if T.length b > keyB
                  then T.take (keyB - 1) b <> "$"
                  else b
    fmm a b c = fmt (keyM + 1) a $ fmt0 keyB (truncatem b) (truncatem c)
    areaCaption t = fmm t "LMB (left mouse button)" "RMB (right mouse button)"
    keySel :: (forall a. (a, a) -> a) -> K.KM
           -> [(CmdArea, KeyOrSlot, Text)]
    keySel sel key =
      let cmd = case M.lookup key bcmdMap of
            Just (_, _, cmd2) -> cmd2
            Nothing -> error $ "" `showFailure` key
          caCmds = case cmd of
            ByAimMode AimModeCmd{exploration=ByArea lexp, aiming=ByArea laim} ->
              sort $ sel (lexp, laim \\ lexp)
            _ -> error $ "" `showFailure` cmd
          caMakeChoice (ca, cmd2) =
            let (km, desc) = case M.lookup cmd2 brevMap of
                  Just ks ->
                    let descOfKM km2 = case M.lookup km2 bcmdMap of
                          Just (_, "", _) -> Nothing
                          Just (_, desc2, _) -> Just (km2, desc2)
                          Nothing -> error $ "" `showFailure` km2
                    in case mapMaybe descOfKM ks of
                      [] -> error $ "" `showFailure` (ks, cmd2)
                      kmdesc3 : _ -> kmdesc3
                  Nothing -> (key, "(not described:" <+> tshow cmd2 <> ")")
            in (ca, Left km, desc)
      in map caMakeChoice caCmds
    doubleIfSquare n | isSquareFont monoFont = 2 * n
                     | otherwise = n
    okm :: (forall a. (a, a) -> a) -> K.KM -> K.KM -> [Text] -> OKX
    okm sel key1 key2 header =
      let kst1 = keySel sel key1
          kst2 = keySel sel key2
          f (ca1, Left km1, _) (ca2, Left km2, _) y =
            assert (ca1 == ca2 `blame` (ca1, ca2, km1, km2, kst1, kst2))
              [ (Left km1, ( PointUI (doubleIfSquare $ keyM + 4) y
                           , ButtonWidth monoFont keyB ))
              , (Left km2, ( PointUI (doubleIfSquare $ keyB + keyM + 5) y
                           , ButtonWidth monoFont keyB )) ]
          f c d e = error $ "" `showFailure` (c, d, e)
          kxs = concat $ zipWith3 f kst1 kst2 [1 + length header..]
          menuLeft = map (\(ca1, _, _) -> textToAL $ areaDescription ca1) kst1
          menuMiddle = map (\(_, _, desc) -> textToAL desc) kst1
          menuRight = map (\(_, _, desc) -> textToAL desc) kst2
          y0 = 1 + length header
      in ( EM.unionsWith (++)
             [ typesetInMono $ "" : header
             , EM.singleton monoFont
               $ typesetXY (doubleIfSquare 2, y0) menuLeft
             , EM.singleton propFont
               $ typesetXY (doubleIfSquare $ keyM + 4, y0) menuMiddle
             , EM.singleton propFont
               $ typesetXY (doubleIfSquare $ keyB + keyM + 5, y0) menuRight ]
         , kxs )
    typesetInSquare :: [Text] -> FontOverlayMap
    typesetInSquare =
      EM.singleton squareFont . typesetXY (spLen, 0) . map textToAL
    typesetInMono :: [Text] -> FontOverlayMap
    typesetInMono =
      EM.singleton monoFont . typesetXY (spLen, 0) . map textToAL
    typesetInProp :: [Text] -> FontOverlayMap
    typesetInProp =
      EM.singleton propFont . typesetXY (spLen, 0) . map textToAL
    sideBySide :: [(Text, OKX)] -> [(Text, OKX)]
    sideBySide ((_t1, okx1) : (t2, okx2) : rest) | not (isSquareFont propFont) =
      (t2, sideBySideOKX rwidth 0 okx1 okx2) : sideBySide rest
    sideBySide l = l
  in sideBySide $ concat
    [ if catLength CmdMinimal
         + length movBlurb1 + length movSchema + length movBlurb2
         + length minimalBlurb
         + 6 > rheight then
        [ ( movTextEnd
          , mergeOKX
              (mergeOKX ( typesetInMono ["", casualDescription <+> "(1/2)", ""]
                        , [] )
                        (mergeOKX (typesetInProp movBlurb1, [])
                                  (typesetInSquare $ "" : movSchema, [])))
              (typesetInProp $ "" : movBlurb2, []) )
        , ( movTextEnd
          , okxs CmdMinimal
                 ( ["", casualDescription <+> "(2/2)", ""]
                 , minimalBlurb ++ [""]
                 , [keyCaption] )
                 ([], []) ) ]
      else
        [ ( movTextEnd
          , mergeOKX
              (mergeOKX ( typesetInMono ["", casualDescription, ""]
                        , [] )
                        (mergeOKX (typesetInProp movBlurb1, [])
                                  (typesetInSquare $ "" : movSchema, [])))
              (okxs CmdMinimal
                    ( []
                    , [""] ++ movBlurb2 ++ [""]
                       ++ minimalBlurb ++ [""]
                    , [keyCaption] )
                    ([], [""])) ) ]
    , if 45 > rheight then
        [ ( movTextEnd
          , let (ls, _) = okxs CmdMouse
                               ( ["", "Optional mouse commands", ""]
                               , mouseBasicsBlurb ++ [""]
                               , [mouseOverviewCaption] )
                               ([], [])
            in (ls, []) )  -- don't capture mouse wheel, etc.
        , ( movTextEnd
          , mergeOKX
              (typesetInMono $ "" : mouseAreasMini, [])
              (mergeOKX
                 (okm fst K.leftButtonReleaseKM K.rightButtonReleaseKM
                      [areaCaption "Exploration"])
                 (okm snd K.leftButtonReleaseKM K.rightButtonReleaseKM
                      [areaCaption "Aiming Mode"])) ) ]
      else
        [ ( movTextEnd
          , let (ls, _) = okxs CmdMouse
                               ( ["", "Optional mouse commands", ""]
                               , mouseBasicsBlurb ++ [""]
                               , [mouseOverviewCaption] )
                               ([], [])
                okx0 = (ls, [])  -- don't capture mouse wheel, etc.
            in mergeOKX
                 (mergeOKX
                    okx0
                    (typesetInProp $ "" : mouseAreasBlurb, []))
                 (mergeOKX
                    (okm fst K.leftButtonReleaseKM K.rightButtonReleaseKM
                         [areaCaption "Exploration"])
                    (okm snd K.leftButtonReleaseKM K.rightButtonReleaseKM
                         [areaCaption "Aiming Mode"] )) ) ]
    , if catLength CmdItem + catLength CmdMove + 9 + 9 > rheight then
        [ ( movTextEnd
          , okxs CmdItem
                 (["", categoryDescription CmdItem], [], ["", keyCaption])
                 ([], "" : itemAllEnding) )
        , ( movTextEnd
          , okxs CmdMove
                 (["", categoryDescription CmdMove], [], ["", keyCaption])
                 (pickLeaderDescription, []) ) ]
      else
        [ ( movTextEnd
          , mergeOKX
              (okxs CmdItem
                    (["", categoryDescription CmdItem], [], ["", keyCaption])
                    ([], "" : itemAllEnding))
              (okxs CmdMove
                    ( ["", "", categoryDescription CmdMove]
                    , []
                    , ["", keyCaption] )
                    (pickLeaderDescription, [""])) ) ]
    , if catLength CmdAim + catLength CmdMeta + 9 > rheight then
        [ ( movTextEnd
          , okxs CmdAim
                 (["", categoryDescription CmdAim], [], ["", keyCaption])
                 ([], []) )
        , ( lastHelpEnd
          , okxs CmdMeta
                 (["", categoryDescription CmdMeta], [], ["", keyCaption])
                 ([], ["", seeAlso]) ) ]
      else
        [ ( lastHelpEnd
          , mergeOKX
              (okxs CmdAim
                    (["", categoryDescription CmdAim], [], ["", keyCaption])
                    ([], []))
              (okxs CmdMeta
                    ( ["", "", categoryDescription CmdMeta]
                    , []
                    , ["", keyCaption] )
                    ([], ["", seeAlso, ""])) ) ]
    ]

-- | Turn the specified portion of bindings into a menu.
--
-- The length of the button may be wrong if the two supplied fonts
-- have very different widths.
okxsN :: InputContent -> DisplayFont -> DisplayFont -> Int -> (HumanCmd -> Bool)
      -> Bool -> CmdCategory -> ([Text], [Text], [Text]) -> ([Text], [Text])
      -> OKX
okxsN InputContent{..} labFont descFont offsetCol2 greyedOut
      showManyKeys cat (headerMono1, headerProp, headerMono2)
      (footerMono, footerProp) =
  let fmt k h = (T.singleton '\x00a0' <> k, h)
      coImage :: HumanCmd -> [K.KM]
      coImage cmd = M.findWithDefault (error $ "" `showFailure` cmd) cmd brevMap
      disp = T.intercalate " or " . map (T.pack . K.showKM)
      keyKnown km = case K.key km of
        K.Unknown{} -> False
        _ -> True
      keys :: [(KeyOrSlot, (Bool, (Text, Text)))]
      keys = [ (Left km, (greyedOut cmd, fmt keyNames desc))
             | (_, (cats, desc, cmd)) <- bcmdList
             , let kms = coImage cmd
                   knownKeys = filter keyKnown kms
                   keyNames =
                     disp $ (if showManyKeys then id else take 1) knownKeys
                   kmsRes = if desc == "" then knownKeys else kms
                   km = case kmsRes of
                     [] -> K.escKM
                     km1 : _ -> km1
             , cat `elem` cats
             , desc /= "" || CmdInternal `elem` cats]
      spLen = textSize labFont " "
      f (ks, (_, (_, t2))) y =
        (ks, ( PointUI spLen y
             , ButtonWidth labFont (offsetCol2 + 2 + T.length t2 - 1)))
      kxs = zipWith f keys
              [length headerMono1 + length headerProp + length headerMono2 ..]
      ts = map (\t -> (False, (t, ""))) headerMono1
           ++ map (\t -> (False, ("", t))) headerProp
           ++ map (\t -> (False, (t, ""))) headerMono2
           ++ map snd keys
           ++ map (\t -> (False, (t, ""))) footerMono
           ++ map (\t -> (False, ("", t))) footerProp
      greyToAL (b, (t1, t2)) =
        if b
        then let al1 = textFgToAL Color.BrBlack t1
             in (al1, ( if T.null t1 then 0 else spLen * (offsetCol2 + 2)
                      , textFgToAL Color.BrBlack t2 ))
        else let al1 = textToAL t1
             in (al1, ( if T.null t1 then 0 else spLen * (offsetCol2 + 2)
                      , textToAL t2 ))
      (greyLab, greyDesc) = unzip $ map greyToAL ts
  in ( EM.insertWith (++) descFont (offsetOverlayX greyDesc)
       $ EM.singleton labFont (offsetOverlay greyLab)
     , kxs )
