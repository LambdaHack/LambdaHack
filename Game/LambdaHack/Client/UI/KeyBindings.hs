-- | Binding of keys to commands.
-- No operation in this module involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.UI.KeyBindings
  ( Binding(..), stdBinding, keyHelp
  ) where

import Control.Arrow (second)
import Control.Exception.Assert.Sugar
import Data.List
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Misc

-- | Bindings and other information about human player commands.
data Binding = Binding
  { bcmdMap  :: !(M.Map K.KM (Text, [CmdCategory], HumanCmd))
                                          -- ^ binding of keys to commands
  , bcmdList :: ![(K.KM, (Text, [CmdCategory], HumanCmd))]
                                          -- ^ the properly ordered list
                                          --   of commands for the help menu
  , brevMap  :: !(M.Map HumanCmd [K.KM])  -- ^ and from commands to their keys
  }

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: KeyKind  -- ^ default key bindings from the content
           -> Config   -- ^ game config
           -> Binding  -- ^ concrete binding
stdBinding copsClient !Config{configCommands, configVi, configLaptop} =
  let cmdWithHelp = rhumanCommands copsClient ++ configCommands
      cmdAll =
        cmdWithHelp
        ++ [ (K.mkKM "KP_Begin", ([CmdMove], Wait))
           , (K.mkKM "CTRL-KP_Begin", ([CmdMove], Alias "" Wait))
           , (K.mkKM "KP_5", ([CmdMove], Alias "" Wait))
           , (K.mkKM "CTRL-KP_5", ([CmdMove], Alias "" Wait)) ]
        ++ (if | configVi ->
                 [ (K.mkKM "period", ([CmdMove], Alias "" Wait)) ]
               | configLaptop ->
                 [ (K.mkKM "i", ([CmdMove], Alias "" Wait))
                 , (K.mkKM "I", ([CmdMove], Alias "" Wait)) ]
               | otherwise ->
                 [])
        ++ K.moveBinding configVi configLaptop (\v -> ([CmdMove], Move v))
                                               (\v -> ([CmdMove], Run v))
      mkDescribed (cats, cmd) = (cmdDescription cmd, cats, cmd)
  in Binding
  { bcmdMap = M.fromList $ map (second mkDescribed) cmdAll
  , bcmdList = map (second mkDescribed) cmdWithHelp
  , brevMap = M.fromListWith (flip (++))
      [ (cmd2, [k])
      | (k, (cats, cmd)) <- cmdAll
      , let cmd2 = case cmd of
              Alias _ cmd1 -> cmd1
              _ -> cmd
      , all (`notElem` [CmdMainMenu, CmdSettingsMenu, CmdDebug, CmdInternal])
            cats ]
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> [OKX]
keyHelp Binding{..} =
  let
    movBlurb =
      [ "Walk throughout a level with mouse or numeric keypad (left diagram)"
      , "or its compact laptop replacement (middle) or the Vi text editor keys"
      , "(right, also known as \"Rogue-like keys\"; can be enabled in config.ui.ini)."
      , "Run, until disturbed, with left mouse button or SHIFT (or CTRL) and a key."
      , ""
      , "               7 8 9          7 8 9          y k u"
      , "                \\|/            \\|/            \\|/"
      , "               4-5-6          u-i-o          h-.-l"
      , "                /|\\            /|\\            /|\\"
      , "               1 2 3          j k l          b j n"
      , ""
      , "In aiming mode (KEYPAD_* or \\) the same keys (or mouse) move the crosshair."
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
    lastBlurb =
      [ ""
      , "For more playing instructions see file PLAYING.md."
      , "Press PGUP to return to previous pages"
      , "or ESC to see the map again."
      ]
    pickLeaderDescription =
      [ fmt 16 "0, 1 ... 6" "pick a particular actor as the new leader"
      ]
    casualDescription = "Minimal cheat sheet for casual play"
    fmt n k h = T.justifyRight 72 ' '
                $ T.justifyLeft n ' ' k
                  <+> T.justifyLeft 48 ' ' h
    fmts s = " " <> T.justifyLeft 71 ' ' s
    movText = map fmts movBlurb
    minimalText = map fmts minimalBlurb
    casualEndText = map fmts casualEndBlurb
    categoryText = map fmts categoryBlurb
    lastText = map fmts lastBlurb
    coImage :: HumanCmd -> [K.KM]
    coImage cmd = M.findWithDefault (assert `failure` cmd) cmd brevMap
    disp cmd = T.concat $ intersperse " or " $ map K.showKM $ coImage cmd
    keysN n cat = [ (Left k, fmt n (disp cmd) h)
                  | (k, (h, cats, cmd)) <- bcmdList, cat `elem` cats, h /= "" ]
    -- TODO: measure the longest key sequence and set the caption automatically
    keyCaptionN n = fmt n "keys" "command"
    keyCaption = keyCaptionN 16
    okxsN :: Int -> CmdCategory -> [Text] -> [Text] -> OKX
    okxsN n cat header footer =
      let (ks, keyTable) = unzip $ keysN n cat
          kxs = zip ks [(y, 0, maxBound) | y <- [length header..]]
      in (toOverlay $ header ++ keyTable ++ footer, kxs)
    okxs = okxsN 16
  in
    [ ( toOverlay $
          [casualDescription <+> "(1/2). [press SPACE to see more]"]
          ++ [""] ++ movText ++ [moreMsg]
      , [(Left K.spaceKM, (length movText + 1, 0, maxBound))])
    , okxs CmdMinimal
        ([casualDescription <+> "(2/2). [press SPACE to see all commands]"]
         ++ [""] ++ minimalText ++ [keyCaption])
        (casualEndText ++ [moreMsg])
    , okxsN 10 CmdMove
        (["All terrain exploration and alteration commands"
         <> ". [press SPACE to advance]"]
         ++ [""] ++ [keyCaptionN 10])
        (categoryText ++ [moreMsg])
    , okxsN 10 CmdItem
        ([categoryDescription CmdItem <> ". [press SPACE to advance]"]
         ++ [""] ++ [keyCaptionN 10])
        (categoryText ++ [moreMsg])
    , okxs CmdTgt
        ([categoryDescription CmdTgt <> ". [press SPACE to advance]"]
         ++ [""] ++ [keyCaption])
        (categoryText ++ [moreMsg])
    , okxs CmdMeta
        ([categoryDescription CmdMeta <> ". [press SPACE to advance]"]
         ++ [""] ++ [keyCaption])
        (pickLeaderDescription ++ categoryText ++ [moreMsg])
    , let (ov, _) =
            okxsN 21 CmdMouse
              ([categoryDescription CmdMouse
               <> ". [press PGUP to see previous, ESC to cancel]"]
               ++ [""] ++ [keyCaptionN 21])
              (lastText ++ [endMsg])
          len = 4 + length (keysN 0 CmdMouse)
      in ( ov
         , [ (Left K.pgupKM, (len + 1, 0, maxBound))
           , (Left K.escKM, (len + 2, 0, maxBound))] )
    ]
