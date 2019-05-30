{-# LANGUAGE RankNTypes, TupleSections #-}
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
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Definition.Color as Color

-- | Produce a set of help/menu screens from the key bindings.
--
-- When the intro screen mentions KP_5, this really is KP_Begin,
-- but since that is harder to understand we assume a different, non-default
-- state of NumLock in the help text than in the code that handles keys.
keyHelp :: COps -> CCUI -> DisplayFont -> [(Text, OKX)]
keyHelp COps{corule} CCUI{ coinput=coinput@InputContent{..}
                         , coscreen=ScreenContent{..} } displayFont =
  let
    introBlurb =
      ""
      : map T.pack rintroScreen
      ++
      [ ""
      , "Press SPACE or PGDN for help and ESC to see the map again."
      ]
    movBlurb = map T.pack rmoveKeysScreen
    movBlurbEnd =
      [ "Press SPACE or scroll the mouse wheel to see the minimal command set."
      ]
    minimalBlurb =
      [ "The following commands, joined with the basic set above,"
      , "let you accomplish anything in the game, though"
      , "not necessarily with the fewest keystrokes. You can also"
      , "play the game exclusively with a mouse, or both mouse"
      , "and keyboard. (See the ending help screens for mouse commands.)"
      , "Lastly, you can select a command with arrows or mouse directly"
      , "from the help screen or the dashboard and execute it on the spot."
      , ""
      ]
    casualEnding =
      [ ""
      , "Press SPACE to see the detailed descriptions of all commands."
      ]
    categoryEnding =
      [ ""
      , "Press SPACE to see the next page of command descriptions."
      ]
    itemMenuEnding =
      [ ""
      , "Note how lower case item commands (stash item, equip item)"
      , "let you move items into a particular item store."
      , ""
      , "Press SPACE to see the detailed descriptions of other item-related commands."
      ]
    itemRemainingEnding =
      [ ""
      , "Note how upper case item commands (manage Inventory, manage Outfit)"
      , "let you view and organize items within a particular item store."
      , "Once a store management menu is opened, you can switch stores"
      , "at will with '<' and '>', so the multiple commands only"
      , "determine the starting item store. Each store is accessible"
      , "from the dashboard as well."
      , ""
      , "Press SPACE to see the next page of command descriptions."
      ]
    itemAllEnding =
      [ ""
      , "Note how lower case item commands (stash item, equip item)"
      , "let you move items into a particular item store, while"
      , "upper case item commands (manage Inventory, manage Outfit)"
      , "let you view and organize items within a particular item store."
      , "Once a store management menu is opened, you can switch stores"
      , "at will with '<' and '>', so the multiple commands only"
      , "determine the starting item store. Each store is accessible"
      , "from the dashboard as well."
      , ""
      , "Press SPACE to see the next page of command descriptions."
      ]
    mouseBasicsBlurb =
      [ "Screen area and UI mode (exploration/aiming) determine"
      , "mouse click effects. First, we give an overview"
      , "of effects of each button over the game map area."
      , "The list includes not only left and right buttons, but also"
      , "the optional middle mouse button (MMB) and the mouse wheel,"
      , "which is also used over menus, to page-scroll them."
      , "(For mice without RMB, one can use Control key with LMB and for mice"
      , "without MMB, one can use C-RMB or C-S-LMB.)"
      , "Next we show mouse button effects per screen area,"
      , "in exploration mode and (if different) in aiming mode."
      , ""
      ]
    mouseBasicsEnding =
      [ ""
      , "Press SPACE to see mouse commands in exploration and aiming modes."
      ]
    lastHelpEnding =
      [ ""
      , "For more playing instructions see file PLAYING.md. Press PGUP or scroll"
      , "mouse wheel for previous pages and press SPACE or ESC to see the map again."
      ]
    keyL = 12
    pickLeaderDescription =
      [ fmt keyL "0, 1 ... 9" "pick a particular actor as the new pointman"
      ]
    casualDescription = "Minimal cheat sheet for casual play"
    fmt n k h = " " <> T.justifyLeft n ' ' k <+> h
    fmts s = " " <> s
    introText = map fmts introBlurb
    movText = map fmts movBlurb
    movTextEnd = map fmts movBlurbEnd
    minimalText = map fmts minimalBlurb
    casualEnd = map fmts casualEnding
    categoryEnd = map fmts categoryEnding
    itemMenuEnd = map fmts itemMenuEnding
    itemRemainingEnd = map fmts itemRemainingEnding
    itemAllEnd = map fmts itemAllEnding
    mouseBasicsText = map fmts mouseBasicsBlurb
    mouseBasicsEnd = map fmts mouseBasicsEnding
    lastHelpEnd = map fmts lastHelpEnding
    keyCaptionN n = fmt n "keys" "command"
    keyCaption = keyCaptionN keyL
    okxs = okxsN coinput displayFont 0 keyL (const False) True
    renumber dy (km, (K.PointUI x y, len)) = (km, (K.PointUI x (y + dy), len))
    renumberOv dy = map (\(K.PointUI x y, al) -> (K.PointUI x (y + dy), al))
    mergeOKX :: OKX -> OKX -> OKX
    mergeOKX (ovs1, ks1) (ovs2, ks2) =
      let off = EM.foldr (\ov acc -> max acc (length ov)) 0 ovs1
      in ( EM.unionWith (\ov1 ov2 -> ov1 ++ renumberOv off ov2) ovs1 ovs2
         , ks1 ++ map (renumber off) ks2 )
    catLength cat = length $ filter (\(_, (cats, desc, _)) ->
      cat `elem` cats && (desc /= "" || CmdInternal `elem` cats)) bcmdList
    keyM = 13
    keyB = 31
    truncatem b = if T.length b > keyB
                  then T.take (keyB - 1) b <> "$"
                  else b
    fmm a b c = fmt keyM a $ fmt keyB (truncatem b) (" " <> truncatem c)
    areaCaption t = fmm t "LMB (left mouse button)" "RMB (right mouse button)"
    keySel :: (forall a. (a, a) -> a) -> K.KM
           -> [(CmdArea, Either K.KM SlotChar, Text)]
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
    doubleIfSquare n | isSquareFont displayFont = 2 * n
                     | otherwise = n
    okm :: (forall a. (a, a) -> a)
        -> K.KM -> K.KM -> [Text] -> [Text]
        -> OKX
    okm sel key1 key2 header footer =
      let kst1 = keySel sel key1
          kst2 = keySel sel key2
          f (ca1, Left km1, _) (ca2, Left km2, _) y =
            assert (ca1 == ca2 `blame` (kst1, kst2))
              [ (Left [km1], ( K.PointUI (doubleIfSquare $ keyM + 3) y
                             , ButtonWidth displayFont keyB ))
              , (Left [km2], ( K.PointUI (doubleIfSquare $ keyB + keyM + 5) y
                             , ButtonWidth displayFont keyB )) ]
          f c d e = error $ "" `showFailure` (c, d, e)
          kxs = concat $ zipWith3 f kst1 kst2 [1 + length header..]
          render (ca1, _, desc1) (_, _, desc2) =
            fmm (areaDescription ca1) desc1 desc2
          menu = zipWith render kst1 kst2
      in (toDisplayFont $ "" : header ++ menu ++ footer, kxs)
    toDisplayFont :: [Text] -> FontOverlayMap
    toDisplayFont = EM.singleton displayFont . offsetOverlay . map textToAL
  in concat
    [ [ ( rtitle corule <+> "- backstory"
        , (toDisplayFont introText, []) ) ]
    , if catLength CmdMinimal
         + length movText + length minimalText + length casualEnd
         + 5 > rheight then
        [ ( casualDescription <+> "(1/2)."
          , (toDisplayFont $ [""] ++ movText ++ [""] ++ movTextEnd, []) )
        , ( casualDescription <+> "(2/2)."
          , okxs CmdMinimal (minimalText ++ [keyCaption]) casualEnd ) ]
      else
        [ ( casualDescription <> "."
          , okxs CmdMinimal
                 (movText ++ [""] ++ minimalText ++ [keyCaption])
                 casualEnd ) ]
    , if catLength CmdItemMenu + catLength CmdItem
         + 14 > rheight then
        [ ( categoryDescription CmdItemMenu <> "."
          , okxs CmdItemMenu [keyCaption] itemMenuEnd )
        , ( categoryDescription CmdItem <> "."
          , okxs CmdItem [keyCaption] itemRemainingEnd ) ]
      else
        [ ( categoryDescription CmdItemMenu <> "."
          , mergeOKX
              (okxs CmdItemMenu [keyCaption] [""])
              (okxs CmdItem
                    [categoryDescription CmdItem <> ".", "", keyCaption]
                    itemAllEnd) ) ]
    , if catLength CmdMove + catLength CmdAim
         + 9 > rheight then
        [ ( "All terrain exploration and modification commands."
          , okxs CmdMove [keyCaption] (pickLeaderDescription ++ categoryEnd) )
        , ( categoryDescription CmdAim <> "."
          , okxs CmdAim [keyCaption] categoryEnd ) ]
      else
        [ ( "All terrain exploration and modification commands."
          , mergeOKX
              (okxs CmdMove [keyCaption] (pickLeaderDescription ++ [""]))
              (okxs CmdAim
                    [categoryDescription CmdAim <> ".", "", keyCaption]
                    categoryEnd) ) ]
    , if 45 > rheight then
        [ ( "Mouse overview."
          , let (ls, _) = okxs CmdMouse
                               (mouseBasicsText ++ [keyCaption])
                               mouseBasicsEnd
            in (ls, []) )  -- don't capture mouse wheel, etc.
        , ( "Mouse in exploration and aiming modes."
          , mergeOKX
               (okm fst K.leftButtonReleaseKM K.rightButtonReleaseKM
                    [areaCaption "exploration"] [])
               (okm snd K.leftButtonReleaseKM K.rightButtonReleaseKM
                    [areaCaption "aiming mode"] categoryEnd) ) ]
      else
        [ ( "Mouse commands."
          , let (ls, _) = okxs CmdMouse
                               (mouseBasicsText ++ [keyCaption])
                               []
                okx0 = (ls, [])  -- don't capture mouse wheel, etc.
            in mergeOKX
                 (mergeOKX
                    okx0
                    (okm fst K.leftButtonReleaseKM K.rightButtonReleaseKM
                         [areaCaption "exploration"] []))
                 (okm snd K.leftButtonReleaseKM K.rightButtonReleaseKM
                      [areaCaption "aiming mode"] categoryEnd) ) ]
    , [ ( categoryDescription CmdMeta <> "."
        , okxs CmdMeta [keyCaption] lastHelpEnd ) ]
    ]

-- | Turn the specified portion of bindings into a menu.
okxsN :: InputContent -> DisplayFont -> Int -> Int -> (HumanCmd -> Bool) -> Bool
      -> CmdCategory
      -> [Text] -> [Text] -> OKX
okxsN InputContent{..} displayFont offset n greyedOut showManyKeys cat
      header footer =
  let fmt k h = " " <> T.justifyLeft n ' ' k <+> h
      coImage :: HumanCmd -> [K.KM]
      coImage cmd = M.findWithDefault (error $ "" `showFailure` cmd) cmd brevMap
      disp = T.intercalate " or " . map (T.pack . K.showKM)
      keyKnown km = case K.key km of
        K.Unknown{} -> False
        _ -> True
      keys :: [(Either [K.KM] SlotChar, (Bool, Text))]
      keys = [ (Left kmsRes, (greyedOut cmd, fmt keyNames desc))
             | (_, (cats, desc, cmd)) <- bcmdList
             , let kms = coImage cmd
                   knownKeys = filter keyKnown kms
                   keyNames =
                     disp $ (if showManyKeys then id else take 1) knownKeys
                   kmsRes = if desc == "" then knownKeys else kms
             , cat `elem` cats
             , desc /= "" || CmdInternal `elem` cats]
      spLen = textSize displayFont " "
      f (ks, (_, tkey)) y = (ks, ( K.PointUI spLen y
                                 , ButtonWidth displayFont (T.length tkey - 1)))
      kxs = zipWith f keys [offset + 1 + length header..]
      renumberOv = map (\(K.PointUI x y, al) -> (K.PointUI x (y + offset), al))
      ts = map (False,) ("" : header) ++ map snd keys ++ map (False,) footer
      greyToAL (b, t) = if b then textFgToAL Color.BrBlack t else textToAL t
      greyTs = map greyToAL ts
  in (EM.singleton displayFont $ renumberOv $ offsetOverlay greyTs, kxs)
