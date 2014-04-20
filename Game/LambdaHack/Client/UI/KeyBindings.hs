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
  let heroSelect k = ( K.KM { key=K.Char (Char.intToDigit k)
                            , modifier=K.NoModifier }
                     , ([CmdMeta], PickLeader k) )
      cmdWithHelp = rhumanCommands copsClient ++ configCommands
      cmdAll =
        cmdWithHelp
        ++ [(K.mkKM "KP_Begin", ([CmdMove], Wait))]
        ++ K.moveBinding configVi configLaptop (\v -> ([CmdMove], Move v))
                                               (\v -> ([CmdMove], Run v))
        ++ fmap heroSelect [0..6]
      mkDescribed (cats, cmd) = (cmdDescription cmd, cats, cmd)
  in Binding
  { bcmdMap = M.fromList $ map (second mkDescribed) cmdAll
  , bcmdList = map (second mkDescribed) cmdWithHelp
  , brevMap = M.fromList $ map swap $ map (second snd) cmdAll
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Slideshow
keyHelp Binding{bcmdList} =
  let
    minimalBlurb =
      [ "Move throughout a level with numerical keypad (left diagram)"
      , "or its compact laptop replacement (middle) or Vi text editor keys"
      , "(right, also known as \"Rogue-like keys\"; can be enabled in config.ui.ini)."
      , "Run ahead, until anything disturbs you, with SHIFT (or CTRL) and a key."
      , ""
      , "               7 8 9          7 8 9          y k u"
      , "                \\|/            \\|/            \\|/"
      , "               4-5-6          u-i-o          h-.-l"
      , "                /|\\            /|\\            /|\\"
      , "               1 2 3          j k l          b j n"
      , ""
      , "Press SPACE to see detailed command descriptions."
      ]
    movBlurb =
      [ "Move throughout a level with numerical keypad (left diagram)"
      , "or its compact laptop replacement (middle) or Vi text editor keys"
      , "(right, also known as \"Rogue-like keys\"; can be enabled in config.ui.ini)."
      , "Run ahead, until anything disturbs you, with SHIFT (or CTRL) and a key."
      , ""
      , "               7 8 9          7 8 9          y k u"
      , "                \\|/            \\|/            \\|/"
      , "               4-5-6          u-i-o          h-.-l"
      , "                /|\\            /|\\            /|\\"
      , "               1 2 3          j k l          b j n"
      , ""
      , "In targeting mode the very same keys move the targeting cursor."
      , "Press '5' or 'i' or '.' to wait, bracing for blows, which reduces"
      , "any damage taken and makes it impossible for foes to displace you."
      , "You displace enemies or friends by bumping into them with SHIFT (or CTRL)."
      , ""
      , "Search, loot, open and attack by bumping into walls, doors and enemies."
      , "The best object to attack with is automatically chosen from among"
      , "weapons in your personal inventory and your unwounded body parts."
      , ""
      , "Press SPACE to see command descriptions."
      ]
    categoryBlurb =
      [ ""
      , "Press SPACE to see the next page of command descriptions."
      ]
    lastBlurb =
      [ ""
      , "For more playing instructions see file PLAYING.md."
      , "Press SPACE to clear the messages and see the map again."
      ]
    fmt k h = T.justifyRight 72 ' '
              $ T.justifyLeft 15 ' ' k
                <> T.justifyLeft 41 ' ' h
    fmts s = " " <> T.justifyLeft 71 ' ' s
    minimalText = map fmts minimalBlurb
    movText = map fmts movBlurb
    categoryText = map fmts categoryBlurb
    lastText = map fmts lastBlurb
    keyCaption = fmt "keys" "command"
    coImage :: K.KM -> [K.KM]
    coImage k = k : sort [ from
                         | (from, (_, _, Macro _ [to])) <- bcmdList
                         , K.mkKM to == k ]
    disp k = T.concat $ intersperse " and " $ map K.showKM $ coImage k
    keys cat = [ fmt (disp k) h
               | (k, (h, cats, _)) <- bcmdList, cat `elem` cats, h /= "" ]
  in toSlideshow True
    [ [categoryDescription CmdMinimal
       <> ". [press SPACE to see all commands]"] ++ [""]
      ++ minimalText ++ [moreMsg]
    , ["Movement. [press SPACE to advance]"] ++ [""]
      ++ movText ++ [moreMsg]
    , [categoryDescription CmdMove <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaption] ++ keys CmdMove ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdItem <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaption] ++ keys CmdItem ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdTgt <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaption] ++ keys CmdTgt ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdAuto <> ". [press SPACE to advance]"] ++ [""]
      ++ [keyCaption] ++ keys CmdAuto ++ categoryText ++ [moreMsg]
    , [categoryDescription CmdMeta <> "."] ++ [""]
      ++ [keyCaption] ++ keys CmdMeta ++ lastText
    ]
