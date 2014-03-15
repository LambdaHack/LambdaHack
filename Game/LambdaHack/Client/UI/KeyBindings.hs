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

import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Common.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.Msg

-- | Bindings and other information about human player commands.
data Binding = Binding
  { bcmdMap  :: !(M.Map K.KM (Text, CmdCategory, HumanCmd))
                                        -- ^ binding of keys to commands
  , bcmdList :: ![(K.KM, (Text, CmdCategory, HumanCmd))]
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
                     , (CmdMeta, PickLeader k) )
      cmdWithHelp = rhumanCommands copsClient ++ configCommands
      cmdAll =
        cmdWithHelp
        ++ [(K.mkKM "KP_Begin", (CmdMove, Wait))]
        ++ K.moveBinding configVi configLaptop (\v -> (CmdMove, Move v))
                                               (\v -> (CmdMove, Run v))
        ++ fmap heroSelect [0..6]
      mkDescribed (cat, cmd) = (cmdDescription cmd, cat, cmd)
  in Binding
  { bcmdMap = M.fromList $ map (second mkDescribed) cmdAll
  , bcmdList = map (second mkDescribed) cmdWithHelp
  , brevMap = M.fromList $ map swap $ map (second snd) cmdAll
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Slideshow
keyHelp Binding{bcmdList} =
  let
    movBlurb =
      [ "Move throughout the level with numerical keypad (left diagram)"
      , "or its compact laptop replacement (middle) or Vi text editor keys"
      , "(right, also known as \"Rogue-like keys\"; can be enabled in config.ui.ini):"
      , ""
      , "               7 8 9          7 8 9          y k u"
      , "                \\|/            \\|/            \\|/"
      , "               4-5-6          u-i-o          h-.-l"
      , "                /|\\            /|\\            /|\\"
      , "               1 2 3          j k l          b j n"
      , ""
      , "Run ahead, until anything disturbs you, with SHIFT (or CTRL) and a key."
      , "Press '5' or 'i' or '.' to wait, bracing for blows next turn."
      , "In targeting mode the same keys move the targeting cursor."
      , ""
      , "Search, open and attack by bumping into walls, doors and enemies."
      , ""
      , "Press SPACE to see detailed command descriptions."
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
               | (k, (h, cat', _)) <- bcmdList, cat == cat', h /= "" ]
  in toSlideshow True
    [ ["Basic keys. [press SPACE to advance]"] ++ [""]
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
