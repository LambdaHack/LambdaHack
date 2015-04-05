-- | Binding of keys to commands.
-- No operation in this module involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.UI.KeyBindings
  ( Binding(..), stdBinding, keyHelp
  ) where

import Control.Arrow (second)
import qualified Data.Char as Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Msg

-- | Bindings and other information about human player commands.
data Binding = Binding
  { bcmdMap  :: !(M.Map K.KM (Text, [CmdCategory], HumanCmd))
                                        -- ^ binding of keys to commands
  , bcmdList :: ![(K.KM, (Text, [CmdCategory], HumanCmd))]
                                        -- ^ the properly ordered list
                                        --   of commands for the help menu
  , brevMap  :: !(M.Map HumanCmd K.KM)  -- ^ and from commands to their keys
  }

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: KeyKind  -- ^ default key bindings from the content
           -> Config   -- ^ game config
           -> Binding  -- ^ concrete binding
stdBinding copsClient !Config{configCommands, configVi, configLaptop} =
  let heroSelect k = ( K.toKM K.NoModifier (K.Char (Char.intToDigit k))
                     , ([CmdMeta], PickLeader k) )
      cmdWithHelp = rhumanCommands copsClient ++ configCommands
      cmdAll =
        cmdWithHelp
        ++ [ (K.mkKM "KP_Begin", ([CmdMove], Wait))
           , (K.mkKM "CTRL-KP_Begin", ([CmdMove], Macro "" ["KP_Begin"]))
           , (K.mkKM "KP_5", ([CmdMove], Macro "" ["KP_Begin"]))
           , (K.mkKM "CTRL-KP_5", ([CmdMove], Macro "" ["KP_Begin"])) ]
        ++ (if configVi
            then [ (K.mkKM "period", ([CmdMove], Macro "" ["KP_Begin"])) ]
            else if configLaptop
            then [ (K.mkKM "i", ([CmdMove], Macro "" ["KP_Begin"]))
                 , (K.mkKM "I", ([CmdMove], Macro "" ["KP_Begin"])) ]
            else [])
        ++ K.moveBinding configVi configLaptop (\v -> ([CmdMove], Move v))
                                               (\v -> ([CmdMove], Run v))
        ++ fmap heroSelect [0..6]
      mkDescribed (cats, cmd) = (cmdDescription cmd, cats, cmd)
  in Binding
  { bcmdMap = M.fromList $ map (second mkDescribed) cmdAll
  , bcmdList = map (second mkDescribed) cmdWithHelp
  , brevMap = M.fromList $ map (swap . second snd) cmdAll
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Slideshow
keyHelp Binding{bcmdList} =
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
      , "though not neccessarily with the fewest number of keystrokes."
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
      , "Press PGUP to return to previous pages or ESC to see the map again."
      ]
    pickLeaderDescription =
      [ fmt 16 "0, 1 ... 6" "pick a particular actor as the new leader"
      ]
    casualDescription = "Minimal cheat sheet for casual play"
    fmt n k h = T.justifyRight 72 ' '
                $ T.justifyLeft n ' ' k
                  <> T.justifyLeft 48 ' ' h
    fmts s = " " <> T.justifyLeft 71 ' ' s
    movText = map fmts movBlurb
    minimalText = map fmts minimalBlurb
    casualEndText = map fmts casualEndBlurb
    categoryText = map fmts categoryBlurb
    lastText = map fmts lastBlurb
    coImage :: K.KM -> [K.KM]
    coImage k = k : sort [ from
                         | (from, (_, cats, Macro _ [to])) <- bcmdList
                         , K.mkKM to == k
                         , any (`notElem` [CmdDebug, CmdInternal]) cats ]
    disp k = T.concat $ intersperse " or " $ map K.showKM $ coImage k
    keysN n cat = [ fmt n (disp k) h
                  | (k, (h, cats, _)) <- bcmdList, cat `elem` cats, h /= "" ]
    -- TODO: measure the longest key sequence and set the caption automatically
    keyCaptionN n = fmt n "keys" "command"
    keys = keysN 16
    keyCaption = keyCaptionN 16
  in toSlideshow (Just True)
    [ [casualDescription <+> "(1/2). [press SPACE to see more]"] ++ [""]
      ++ movText ++ [moreMsg]
    , [casualDescription <+> "(2/2). [press SPACE to see all commands]"] ++ [""]
      ++ minimalText
      ++ [keyCaption] ++ keys CmdMinimal ++ casualEndText ++ [moreMsg]
    , ["All terrain exploration and alteration commands"
       <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaptionN 10] ++ keysN 10 CmdMove ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdItem <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaptionN 10] ++ keysN 10 CmdItem ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdTgt <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaption] ++ keys CmdTgt ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdAuto <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaption] ++ keys CmdAuto ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdMeta <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaption] ++ keys CmdMeta ++ pickLeaderDescription
      ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdMouse
       <> ". [press PGUP to see previous, ESC to cancel]"] ++ [""]
      ++ [keyCaptionN 21] ++ keysN 21 CmdMouse ++ lastText ++ [endMsg]
    ]
