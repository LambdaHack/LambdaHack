{-# LANGUAGE TupleSections #-}
-- | Verifying, aggregating and displaying binding of keys to commands.
module Game.LambdaHack.Client.UI.KeyBindings
  ( Binding(..), stdBinding, keyHelp, okxsN
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Game.LambdaHack.Client.UI.Content.KeyKind
import           Game.LambdaHack.Client.UI.HumanCmd
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.UIOptions
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Content.RuleKind

-- | Bindings and other information about human player commands.
data Binding = Binding
  { bcmdMap  :: M.Map K.KM CmdTriple   -- ^ binding of keys to commands
  , bcmdList :: [(K.KM, CmdTriple)]    -- ^ the properly ordered list
                                       --   of commands for the help menu
  , brevMap  :: M.Map HumanCmd [K.KM]  -- ^ and from commands to their keys
  }

-- | Create binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: KeyKind    -- ^ default key bindings from the content
           -> UIOptions  -- ^ UI client options
           -> Binding    -- ^ concrete binding
stdBinding (KeyKind copsClient) UIOptions{uCommands, uVi, uLaptop} =
  let waitTriple = ([CmdMove], "", Wait)
      wait10Triple = ([CmdMove], "", Wait10)
      moveXhairOr n cmd v = ByAimMode { exploration = cmd v
                                      , aiming = MoveXhair v n }
      bcmdList =
        (if uVi
         then filter (\(k, _) ->
           k `notElem` [K.mkKM "period", K.mkKM "C-period"])
         else id) copsClient
        ++ uCommands
        ++ [ (K.mkKM "KP_Begin", waitTriple)
           , (K.mkKM "C-KP_Begin", wait10Triple)
           , (K.mkKM "KP_5", waitTriple)
           , (K.mkKM "C-KP_5", wait10Triple) ]
        ++ (if | uVi ->
                 [ (K.mkKM "period", waitTriple)
                 , (K.mkKM "C-period", wait10Triple) ]
               | uLaptop ->
                 [ (K.mkKM "i", waitTriple)
                 , (K.mkKM "C-i", wait10Triple)
                 , (K.mkKM "I", waitTriple) ]
               | otherwise ->
                 [])
        ++ K.moveBinding uVi uLaptop
             (\v -> ([CmdMove], "", moveXhairOr 1 MoveDir v))
             (\v -> ([CmdMove], "", moveXhairOr 10 RunDir v))
      rejectRepetitions t1 t2 = error $ "duplicate key"
                                        `showFailure` (t1, t2)
  in Binding
  { bcmdMap = M.fromListWith rejectRepetitions
      [ (k, triple)
      | (k, triple@(cats, _, _)) <- bcmdList
      , all (`notElem` [CmdMainMenu]) cats
      ]
  , bcmdList
  , brevMap = M.fromListWith (flip (++)) $ concat
      [ [(cmd, [k])]
      | (k, (cats, _desc, cmd)) <- bcmdList
      , all (`notElem` [CmdMainMenu, CmdDebug, CmdNoHelp]) cats
      ]
  }

-- | Produce a set of help/menu screens from the key bindings.
keyHelp :: Kind.COps -> Binding -> Int -> [(Text, OKX)]
keyHelp Kind.COps{corule} keyb@Binding{..} offset = assert (offset > 0) $
  let
    stdRuleset = Kind.stdRuleset corule
    introBlurb =
      ""
      : map T.pack (rintroScreen stdRuleset)
      ++
      [ ""
      , "Press SPACE for help or ESC to see the map again."
      ]
    movBlurb =
      [ ""
      , "Walk throughout a level with mouse or numeric keypad (left diagram below)"
      , "or its compact laptop replacement (middle) or the Vi text editor keys (right,"
      , "enabled in config.ui.ini). Run, until disturbed, by adding Shift or Control."
      , "Go-to with LMB (left mouse button). Run collectively with RMB."
      , ""
      , "               7 8 9          7 8 9          y k u"
      , "                \\|/            \\|/            \\|/"
      , "               4-5-6          u-i-o          h-.-l"
      , "                /|\\            /|\\            /|\\"
      , "               1 2 3          j k l          b j n"
      , ""
      , "In aiming mode, the same keys (and mouse) move the x-hair (aiming crosshair)."
      , "Press 'KP_5' ('5' on keypad, if present) to wait, bracing for impact,"
      , "which reduces any damage taken and prevents displacement by foes. Press"
      , "'C-KP_5' (the same key with Control) to wait 0.1 of a turn, without bracing."
      , "You displace enemies by running into them with Shift/Control or RMB. Search,"
      , "open, descend and attack by bumping into walls, doors, stairs and enemies."
      , "The best item to attack with is automatically chosen from among"
      , "weapons in your personal equipment and your unwounded organs."
      , ""
      , "Press SPACE or scroll the mouse wheel to see the minimal command set."
      ]
    minimalBlurb =
      [ "The following commands, joined with the basic set above, let you accomplish"
      , "anything in the game, though not necessarily with the fewest keystrokes."
      , "You can also play the game exclusively with a mouse, or both mouse and"
      , "keyboard. See the ending help screens for mouse commands."
      , "Lastly, you can select a command with arrows or mouse directly from the help"
      , "screen and execute it on the spot."
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
    lastCategoryEnding =
      [ ""
      , "Press SPACE to see mouse command descriptions."
      ]
    mouseBasicsBlurb =
      [ "Screen area and UI mode (aiming/exploration) determine mouse click effects."
      , "Here is an overview of effects of each button over most of the game map area."
      , "The list includes not only left and right buttons, but also the optional"
      , "middle mouse button (MMB) and even the mouse wheel, which is normally used"
      , "over menus, to page-scroll them, rather than over game map."
      , "For mice without RMB, one can use C-LMB (Control key and left mouse button)."
      , ""
      ]
    mouseBasicsEnding =
      [ ""
      , "Press SPACE to see mouse commands in aiming mode."
      ]
    mouseAimingModeEnding =
      [ ""
      , "Press SPACE to see mouse commands in explorations mode."
      ]
    lastHelpEnding =
      [ ""
      , "For more playing instructions see file PLAYING.md."
      , "Press PGUP or scroll the mouse wheel to return to previous pages"
      , "and press SPACE or ESC to see the map again."
      ]
    keyL = 12
    pickLeaderDescription =
      [ fmt keyL "0, 1 ... 6" "pick a particular actor as the new leader"
      ]
    casualDescription = "Minimal cheat sheet for casual play"
    fmt n k h = " " <> T.justifyLeft n ' ' k <+> h
    fmts s = " " <> s
    introText = map fmts introBlurb
    movText = map fmts movBlurb
    minimalText = map fmts minimalBlurb
    casualEnd = map fmts casualEnding
    categoryEnd = map fmts categoryEnding
    lastCategoryEnd = map fmts lastCategoryEnding
    mouseBasicsText = map fmts mouseBasicsBlurb
    mouseBasicsEnd = map fmts mouseBasicsEnding
    mouseAimingModeEnd = map fmts mouseAimingModeEnding
    lastHelpEnd = map fmts lastHelpEnding
    keyCaptionN n = fmt n "keys" "command"
    keyCaption = keyCaptionN keyL
    okxs = okxsN keyb offset keyL (const False) True
    keyM = 13
    keyB = 31
    truncatem b = if T.length b > keyB
                  then T.take (keyB - 1) b <> "$"
                  else b
    fmm a b c = fmt keyM a $ fmt keyB (truncatem b) (" " <> truncatem c)
    areaCaption = fmm "area" "LMB (left mouse button)"
                             "RMB (right mouse button)"
    keySel :: ((HumanCmd, HumanCmd) -> HumanCmd) -> K.KM
           -> [(CmdArea, Either K.KM SlotChar, Text)]
    keySel sel key =
      let cmd = case M.lookup key bcmdMap of
            Just (_, _, cmd2) -> cmd2
            Nothing -> error $ "" `showFailure` key
          caCmds = case cmd of
            ByAimMode{..} -> case sel (exploration, aiming) of
              ByArea l -> sort l
              _ -> error $ "" `showFailure` cmd
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
    okm :: ((HumanCmd, HumanCmd) -> HumanCmd)
        -> K.KM -> K.KM -> [Text] -> [Text]
        -> OKX
    okm sel key1 key2 header footer =
      let kst1 = keySel sel key1
          kst2 = keySel sel key2
          f (ca1, Left km1, _) (ca2, Left km2, _) y = assert (ca1 == ca2)
            [ (Left [km1], (y, keyM + 3, keyB + keyM + 3))
            , (Left [km2], (y, keyB + keyM + 5, 2 * keyB + keyM + 5)) ]
          f c d e = error $ "" `showFailure` (c, d, e)
          kxs = concat $ zipWith3 f kst1 kst2 [offset + length header..]
          render (ca1, _, desc1) (_, _, desc2) =
            fmm (areaDescription ca1) desc1 desc2
          menu = zipWith render kst1 kst2
      in (map textToAL $ "" : header ++ menu ++ footer, kxs)
  in
    [ ( rtitle stdRuleset <+> "- backstory"
      , (map textToAL introText, []) )
    , ( casualDescription <+> "(1/2)."
      , (map textToAL movText, []) )
    , ( casualDescription <+> "(2/2)."
      , okxs CmdMinimal (minimalText ++ [keyCaption]) casualEnd )
    , ( "All terrain exploration and alteration commands."
      , okxs CmdMove [keyCaption] categoryEnd )
    , ( categoryDescription CmdItemMenu <> "."
      , okxs CmdItemMenu [keyCaption] categoryEnd )
    , ( categoryDescription CmdItem <> "."
      , okxs CmdItem [keyCaption] categoryEnd )
    , ( categoryDescription CmdAim <> "."
      , okxs CmdAim [keyCaption] categoryEnd )
    , ( categoryDescription CmdMeta <> "."
      , okxs CmdMeta [keyCaption] (pickLeaderDescription ++ lastCategoryEnd) )
    , ( "Mouse overview."
      , let (ls, _) =
              okxs CmdMouse (mouseBasicsText ++ [keyCaption]) mouseBasicsEnd
        in (ls, []) )  -- don't capture mouse wheel, etc.
    , ( "Mouse in aiming mode."
      , okm snd K.leftButtonReleaseKM K.rightButtonReleaseKM
            [areaCaption] mouseAimingModeEnd )
    , ( "Mouse in exploration mode."
      , okm fst K.leftButtonReleaseKM K.rightButtonReleaseKM
            [areaCaption] lastHelpEnd )
    ]

-- | Turn the specified portion of bindings into a menu.
okxsN :: Binding -> Int -> Int -> (HumanCmd -> Bool) -> Bool -> CmdCategory
      -> [Text] -> [Text] -> OKX
okxsN Binding{..} offset n greyedOut showManyKeys cat header footer =
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
      f (ks, (_, tkey)) y = (ks, (y, 1, T.length tkey))
      kxs = zipWith f keys [offset + length header..]
      ts = map (False,) ("" : header) ++ map snd keys ++ map (False,) footer
      greyToAL (b, t) = if b then fgToAL Color.BrBlack t else textToAL t
  in (map greyToAL ts, kxs)
