{-# LANGUAGE TupleSections #-}
-- | Binding of keys to commands.
-- No operation in this module involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.UI.KeyBindings
  ( Binding(..), stdBinding, keyHelp, okxsN
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.Slideshow
import qualified Game.LambdaHack.Common.Color as Color

-- | Bindings and other information about human player commands.
data Binding = Binding
  { bcmdMap  :: !(M.Map K.KM CmdTriple)   -- ^ binding of keys to commands
  , bcmdList :: ![(K.KM, CmdTriple)]      -- ^ the properly ordered list
                                          --   of commands for the help menu
  , brevMap  :: !(M.Map HumanCmd [K.KM])  -- ^ and from commands to their keys
  }

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: KeyKind  -- ^ default key bindings from the content
           -> Config   -- ^ game config
           -> Binding  -- ^ concrete binding
stdBinding copsClient !Config{configCommands, configVi, configLaptop} =
  let waitTriple = ([CmdMove], "", Wait)
      moveXhairOr n cmd v = ByAimMode { exploration = cmd v
                                      , aiming = MoveXhair v n }
      cmdAll =
        rhumanCommands copsClient
        ++ configCommands
        ++ [ (K.mkKM "KP_Begin", waitTriple)
           , (K.mkKM "CTRL-KP_Begin", waitTriple)
           , (K.mkKM "KP_5", waitTriple)
           , (K.mkKM "CTRL-KP_5", waitTriple) ]
        ++ (if | configVi ->
                 [ (K.mkKM "period", waitTriple) ]
               | configLaptop ->
                 [ (K.mkKM "i", waitTriple)
                 , (K.mkKM "I", waitTriple) ]
               | otherwise ->
                 [])
        ++ K.moveBinding configVi configLaptop
             (\v -> ([CmdMove], "", moveXhairOr 1 MoveDir v))
             (\v -> ([CmdMove], "", moveXhairOr 10 RunDir v))
      rejectRepetitions t1 t2 = assert `failure` "duplicate key"
                                       `twith` (t1, t2)
  in Binding
  { bcmdMap = M.fromListWith rejectRepetitions
      [ (k, triple)
      | (k, triple@(cats, _, _)) <- cmdAll
      , all (`notElem` [CmdMainMenu]) cats
      ]
  , bcmdList = cmdAll
  , brevMap = M.fromListWith (flip (++)) $ concat
      [ [(cmd, [k])]
      | (k, (cats, _desc, cmd)) <- cmdAll
      , all (`notElem` [CmdMainMenu, CmdInternal, CmdDebug, CmdNoHelp]) cats
      ]
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Int -> [(Text, OKX)]
keyHelp keyb@Binding{..} offset = assert (offset > 0) $
  let
    movBlurb =
      [ ""
      , "Walk throughout a level with mouse or numeric keypad (left diagram)"
      , "or its compact laptop replacement (middle) or the Vi text editor keys"
      , "(right, also known as \"Rogue-like keys\"; can be enabled in config.ui.ini)."
      , "Run, until disturbed, with SHIFT/CTRL and a movement key. Go-to with LMB"
      , "(left mouse button). Run collectively with RMB (right mouse button)."
      , ""
      , "               7 8 9          7 8 9          y k u"
      , "                \\|/            \\|/            \\|/"
      , "               4-5-6          u-i-o          h-.-l"
      , "                /|\\            /|\\            /|\\"
      , "               1 2 3          j k l          b j n"
      , ""
      , "In aiming mode (KEYPAD_* or !) the same keys (or mouse) move the crosshair."
      , "Press 'KEYPAD_5' (or 'i' or '.') to wait, bracing for blows, which reduces"
      , "any damage taken and makes it impossible for foes to displace you."
      , "You displace enemies by running into them with SHIFT/CTRL or RMB. Search, "
      , "open, descend and attack by bumping into walls, doors, stairs and enemies. "
      , "The best item to attack with is automatically chosen from among"
      , "weapons in your personal equipment and your unwounded organs."
      , ""
      , "Press SPACE or scroll the mouse wheel to see the minimal command set."
      ]
    minimalBlurb =
      [ "Together with the basic commands above, the following set lets you accomplish"
      , "anything in the game, though not necessarily with the fewest number of"
      , "keystrokes. You can also play the game exclusively with a mouse, or both"
      , "mouse and keyboard. See the ending help screens for mouse commands."
      , "Lastly, you can select a command directly from the help screens."
      , ""
      ]
    casualEndBlurb =
      [ ""
      , "Press SPACE to see the detailed descriptions of all commands."
      ]
    categoryBlurb =
      [ ""
      , "Press SPACE to see the next page of command descriptions."
      ]
    lastCategoryBlurb =
      [ ""
      , "Press SPACE to see mouse command descriptions."
      ]
    mouseBasicsBlurb =
      [ ""
      , "Screen area determines mouse click effect; see above and on the next page."
      , "Below is general overview, including not only left and right,"
      , "but also middle mouse button (MMB) and the mouse wheel."
      ]
    lastHelpBlurb =
      [ ""
      , "For more playing instructions see file PLAYING.md."
      , "Press PGUP or scroll the mouse wheel to return to previous pages"
      , "and press SPACE or ESC to see the map again."
      ]
    keyL = 11
    pickLeaderDescription =
      [ fmt keyL "0, 1 ... 6" "pick a particular actor as the new leader"
      ]
    casualDescription = "Minimal cheat sheet for casual play"
    fmt n k h = " " <> T.justifyLeft n ' ' k <+> h
    fmts s = " " <> s
    movText = map fmts movBlurb
    minimalText = map fmts minimalBlurb
    casualEndText = map fmts casualEndBlurb
    categoryText = map fmts categoryBlurb
    lastCategoryText = map fmts lastCategoryBlurb
    mouseBasicsText = map fmts mouseBasicsBlurb
    lastHelpText = map fmts lastHelpBlurb
    keyCaptionN n = fmt n "keys" "command"
    keyCaption = keyCaptionN keyL
    okxs = okxsN keyb offset keyL (const False)
    keyM = 13
    keyB = 31
    truncatem b = if T.length b > keyB
                  then T.take (keyB - 1) b <> "$"
                  else b
    fmm a b c = fmt keyM a $ fmt keyB (truncatem b) (" " <> truncatem c)
    areaCaption = fmm "area" "LMB" "RMB"
    keySel :: ((HumanCmd, HumanCmd) -> HumanCmd) -> K.KM
           -> [(CmdArea, Either K.KM SlotChar, Text)]
    keySel sel key =
      let cmd = case M.lookup key bcmdMap of
            Just (_, _, cmd2) -> cmd2
            Nothing -> assert `failure` key
          caCmds = case cmd of
            ByAimMode{..} -> case sel (exploration, aiming) of
              ByArea l -> sort l
              _ -> assert `failure` cmd
            _ -> assert `failure` cmd
          caMakeChoice (ca, cmd2) =
            let (km, desc) = case M.lookup cmd2 brevMap of
                  Just ks ->
                    let descOfKM km2 = case M.lookup km2 bcmdMap of
                          Just (_, "", _) -> Nothing
                          Just (_, desc2, _) -> Just (km2, desc2)
                          Nothing -> assert `failure` km2
                    in case mapMaybe descOfKM ks of
                      [] -> assert `failure` (ks, cmd2)
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
          f (ca1, Left km1, _) (ca2, Left km2, _) y = assert (ca1 == ca2) $
            [ (Left [km1], (y, keyM + 3, keyB + keyM + 3))
            , (Left [km2], (y, keyB + keyM + 5, 2 * keyB + keyM + 5)) ]
          f c d e = assert `failure` (c, d, e)
          kxs = concat $ zipWith3 f kst1 kst2 [offset + length header..]
          render (ca1, _, desc1) (_, _, desc2) =
            fmm (areaDescription ca1) desc1 desc2
          menu = zipWith render kst1 kst2
      in (map textToAL $ "" : header ++ menu ++ footer, kxs)
    adjoinOverlay (ov1, kxs1) (ov2, _) = (ov1 ++ ov2, kxs1)
  in
    [ ( casualDescription <+> "(1/2)."
      , (map textToAL $ movText, []) )
    , ( casualDescription <+> "(2/2)."
      , okxs CmdMinimal (minimalText ++ [keyCaption]) casualEndText )
    , ( "All terrain exploration and alteration commands."
      , okxs CmdMove [keyCaption] categoryText )
    , ( categoryDescription CmdItem <> "."
      , okxs CmdItem [keyCaption] categoryText )
    , ( categoryDescription CmdAim <> "."
      , okxs CmdAim [keyCaption] categoryText )
    , ( categoryDescription CmdMeta <> "."
      , okxs CmdMeta [keyCaption] (pickLeaderDescription ++ lastCategoryText) )
    , ( "Mouse in aiming mode."
      , okm snd K.leftButtonReleaseKM K.rightButtonReleaseKM
            [areaCaption] mouseBasicsText
        `adjoinOverlay`
        okxs CmdMouse [keyCaption] [] )
    , ( "Mouse in exploration mode."
      , okm fst K.leftButtonReleaseKM K.rightButtonReleaseKM
            [areaCaption] lastHelpText )
    ]

okxsN :: Binding -> Int -> Int -> (HumanCmd -> Bool) -> CmdCategory
      -> [Text] -> [Text] -> OKX
okxsN Binding{..} offset n greyedOut cat header footer =
  let fmt k h = " " <> T.justifyLeft n ' ' k <+> h
      coImage :: HumanCmd -> [K.KM]
      coImage cmd = M.findWithDefault (assert `failure` cmd) cmd brevMap
      disp = T.intercalate " or " . map (T.pack . K.showKM)
      keys :: [(Either [K.KM] SlotChar, (Bool, Text))]
      keys = [ (Left kms, (greyedOut cmd, fmt (disp kms) desc))
             | (_, (cats, desc, cmd)) <- bcmdList
             , let kms = coImage cmd
             , cat `elem` cats
             , desc /= "" ]
      f (ks, (_, tkey)) y = (ks, (y, 1, T.length tkey))
      kxs = zipWith f keys [offset + length header..]
      ts = map (False,) ("" : header) ++ map snd keys ++ map (False,) footer
      greyToAL (b, t) = if b then fgToAL Color.BrBlack t else textToAL t
  in (map greyToAL ts, kxs)
