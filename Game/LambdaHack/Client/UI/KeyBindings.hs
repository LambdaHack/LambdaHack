-- | Binding of keys to commands.
-- No operation in this module involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.UI.KeyBindings
  ( Binding(..), stdBinding, keyHelp
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
  in Binding
  { bcmdMap = M.fromList cmdAll
  , bcmdList = cmdAll
  , brevMap = M.fromListWith (flip (++)) $ concat
      [ [(cmd, [k])]
      | (k, (cats, _desc, cmd)) <- cmdAll
      , all (`notElem` [CmdMainMenu, CmdSettingsMenu, CmdDebug, CmdNoHelp])
            cats
      ]
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Int -> [(Text, OKX)]
keyHelp Binding{..} offset = assert (offset > 0) $
  let
    movBlurb =
      [ ""
      , "Walk throughout a level with mouse or numeric keypad (left diagram)"
      , "or its compact laptop replacement (middle) or the Vi text editor keys"
      , "(right, also known as \"Rogue-like keys\"; can be enabled in config.ui.ini)."
      , "Run, until disturbed, with LMB (left mouse button) or SHIFT/CTRL and a key."
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
      , "You displace enemies or friends by bumping into them with SHIFT (or CTRL)."
      , ""
      , "Search, loot, open and attack by bumping into walls, doors and enemies."
      , "The best item to attack with is automatically chosen from among"
      , "weapons in your personal equipment and your unwounded organs."
      , ""
      , "Press SPACE to see the minimal command set."
      ]
    minimalBlurb =
      [ "The following minimal command set lets you accomplish anything in the game,"
      , "though not necessarily with the fewest number of keystrokes."
      , "Most of the other commands are shorthands, defined as macros"
      , "(with the exception of the advanced commands for assigning non-default"
      , "tactics and targets to your autonomous henchmen, if you have any)."
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
      , "Below is general overview, includng not only left and right,"
      , "but also middle mouse button (MMB) and the mouse wheel."
      ]
    lastHelpBlurb =
      [ ""
      , "For more playing instructions see file PLAYING.md."
      , "Press PGUP to return to previous pages"
      , "and SPACE or ESC to see the map again."
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
    coImage :: HumanCmd -> [K.KM]
    coImage cmd = M.findWithDefault (assert `failure` cmd) cmd brevMap
    disp cmd = T.concat $ intersperse " or " $ map K.showKM $ coImage cmd
    keysN :: Int -> CmdCategory -> [(Either K.KM SlotChar, Text)]
    keysN n cat = [ (Left k, fmt n (disp cmd) desc)
                  | (k, (cats, desc, cmd)) <- bcmdList
                  , cat `elem` cats
                  , desc /= "" ]
    keyCaptionN n = fmt n "keys" "command"
    keyCaption = keyCaptionN keyL
    okxsN :: Int -> CmdCategory -> [Text] -> [Text] -> OKX
    okxsN n cat header footer =
      let kst = keysN n cat
          f (ks, tkey) y = (ks, (y, 0, T.length tkey))
          kxs = zipWith f kst [offset + length header..]
      in (map toAttrLine $ "" : header ++ map snd kst ++ footer, kxs)
    okxs = okxsN keyL
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
          f (ca1, km1, _) (ca2, km2, _) y = assert (ca1 == ca2) $
            [ (km1, (y, keyM + 3, keyB + keyM + 3))
            , (km2, (y, keyB + keyM + 5, 2 * keyB + keyM + 5)) ]
          kxs = concat $ zipWith3 f kst1 kst2 [offset + length header..]
          render (ca1, _, desc1) (_, _, desc2) =
            fmm (areaDescription ca1) desc1 desc2
          menu = zipWith render kst1 kst2
      in (map toAttrLine $ "" : header ++ menu ++ footer, kxs)
    adjoinOverlay (ov1, kxs1) (ov2, _) = (ov1 ++ ov2, kxs1)
  in
    [ ( ""  -- the first screen is for ItemMenu
      , okxs CmdItemMenu [keyCaption] [] )
    , ( casualDescription <+> "(1/2)."
      , (map toAttrLine $ movText, []) )
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
