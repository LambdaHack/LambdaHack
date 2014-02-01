-- | Binding of keys to commands.
-- No operation in this module involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.Binding
  ( Binding(..), stdBinding, keyHelp,
  ) where

import Control.Arrow (second)
import qualified Data.Char as Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.Msg

-- | Bindings and other information about human player commands.
data Binding = Binding
  { kcmd    :: !(M.Map K.KM (Text, HumanCmd))  -- ^ binding keys to commands
  , kmajor  :: ![K.KM]                         -- ^ major commands
  , kminor  :: ![K.KM]                         -- ^ minor commands
  , krevMap :: !(M.Map HumanCmd K.KM)          -- ^ from commands to their keys
  }

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: ConfigUI  -- ^ game config
           -> Binding   -- ^ concrete binding
stdBinding !ConfigUI{configCommands} =
  let heroSelect k = ( K.KM { key=K.Char (Char.intToDigit k)
                            , modifier=K.NoModifier }
                     , PickLeader k )
      cmdList =
        map (\(x, (_y, z)) -> (x, z)) configCommands
        ++ K.moveBinding Move Run
        ++ fmap heroSelect [0..9]
      mkDescribed cmd = (cmdDescription cmd, cmd)
      mkCommand = second mkDescribed
      semList = map mkCommand cmdList
  in Binding
  { kcmd = M.fromList semList
  , kmajor = map fst $ filter (majorHumanCmd . snd) cmdList
  , kminor = map fst $ filter (minorHumanCmd . snd) cmdList
  , krevMap = M.fromList $ map swap cmdList
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Slideshow
keyHelp Binding{kcmd, kmajor, kminor} =
  let
    movBlurb =
      [ "Move throughout the level with numerical keypad or"
      , "the Vi text editor keys (also known as \"Rogue-like keys\"):"
      , ""
      , "               7 8 9          y k u"
      , "                \\|/            \\|/"
      , "               4-5-6          h-.-l"
      , "                /|\\            /|\\"
      , "               1 2 3          b j n"
      , ""
      ,"Run ahead until anything disturbs you, with SHIFT (or CTRL) and a key."
      , "Press keypad '5' or '.' to wait a turn, bracing for blows next turn."
      , "In targeting mode the same keys move the targeting cursor."
      , ""
      , "Search, open and attack, by bumping into walls, doors and enemies."
      , ""
      , "Press SPACE to see the next page, with the list of major commands."
      ]
    majorBlurb =
      [ ""
      , "Press SPACE to see the next page, with the list of minor commands."
      ]
    minorBlurb =
      [ ""
      , "For more playing instructions see file PLAYING.md."
      , "Press SPACE to clear the messages and see the map again."
      ]
    fmt k h = T.justifyRight 72 ' '
              $ T.justifyLeft 15 ' ' k
                <> T.justifyLeft 41 ' ' h
    fmts s  = " " <> T.justifyLeft 71 ' ' s
    blank   = fmt "" ""
    mov     = map fmts movBlurb
    major   = map fmts majorBlurb
    minor   = map fmts minorBlurb
    keyCaption = fmt "keys" "command"
    assocsCmd = M.assocs kcmd
    coImage :: K.KM -> [K.KM]
    coImage k = k : [ from
                    | (from, (_, Macro _ [to])) <- assocsCmd
                    , K.mkKM to == k ]
    disp k = T.concat $ map K.showKM $ coImage k
    keys l = [ fmt (disp k) h | (k, (h, _)) <- l, h /= "" ]
    (kcMajor, kcRest) =
      partition ((`elem` kmajor) . fst) (M.assocs kcmd)
    (kcMinor, _) =
      partition ((`elem` kminor) . fst) kcRest
  in toSlideshow True -- TODO: 80 below is a hack
    [ [T.justifyLeft 80 ' ' "Basic keys. [press SPACE to advance]"] ++ [blank]
      ++ mov ++ [moreMsg]
    , [T.justifyLeft 80 ' ' "Basic keys. [press SPACE to advance]"] ++ [blank]
      ++ [keyCaption] ++ keys kcMajor ++ major ++ [moreMsg]
    , [T.justifyLeft 80 ' ' "Basic keys."] ++ [blank]
      ++ [keyCaption] ++ keys kcMinor ++ minor
    ]
