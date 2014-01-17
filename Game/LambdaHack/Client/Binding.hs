-- | Generic binding of keys to commands, procesing macros,
-- printing command help. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.Binding
  ( Binding(..), stdBinding, keyHelp,
  ) where

import Control.Arrow (second)
import qualified Data.Char as Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.Msg

-- | Bindings and other information about human player commands.
data Binding = Binding
  { kcmd       :: !(M.Map K.KM (Text, Bool, HumanCmd))
                                          -- ^ binding keys to commands
  , kmacro     :: !(M.Map K.KM [K.KM])    -- ^ macro map
  , kmacroDesc :: !(M.Map K.KM Text)      -- ^ macro description map
  , kmajor     :: ![K.KM]                 -- ^ major commands
  , kminor     :: ![K.KM]                 -- ^ minor commands
  , krevMap    :: !(M.Map HumanCmd K.KM)  -- ^ from cmds to their main keys
  }

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: ConfigUI  -- ^ game config
           -> Binding   -- ^ concrete binding
stdBinding !config@ConfigUI{configMacros, configMacroDesc} =
  let kmacro = M.fromList configMacros
      kmacroDesc = M.fromList configMacroDesc  -- no check vs. kmacro
      heroSelect k = ( K.KM { key=K.Char (Char.intToDigit k)
                            , modifier=K.NoModifier }
                     , PickLeader k )
      cmdList =
        configCommands config
        ++ K.moveBinding Move Run
        ++ fmap heroSelect [0..9]
      mkDescribed cmd = (cmdDescription cmd, noRemoteHumanCmd cmd, cmd)
      mkCommand = second mkDescribed
      semList = map mkCommand cmdList
  in Binding
  { kcmd = M.fromList semList
  , kmacro
  , kmacroDesc
  , kmajor = map fst $ filter (majorHumanCmd . snd) cmdList
  , kminor = map fst $ filter (minorHumanCmd . snd) cmdList
  , krevMap = M.fromList $ map swap cmdList
  }

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Slideshow
keyHelp Binding{kcmd, kmacro, kmacroDesc, kmajor, kminor} =
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
      , "Commands marked with * take time and are blocked on remote levels."
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
    coImage :: M.Map K.KM [K.KM] -> K.KM -> [K.KM]
    coImage macroD k = k : [ from | (from, to) <- M.assocs macroD, to == [k] ]
    disp k  = T.concat $ map K.showKM $ coImage kmacro k
    keys l  = [ fmt (disp k) (h <> if timed then "*" else "")
              | (k, (h, timed, _)) <- l, h /= "" ]
    (kcMajor, kcRest) =
      partition ((`elem` kmajor) . fst) (M.toAscList kcmd)
    (kcMinor, _) =
      partition ((`elem` kminor) . fst) kcRest
    macro2cmd (km, _) = case M.lookup km kmacroDesc of
      Nothing -> Nothing
      Just desc ->
        let timed = True  -- TODO: check if any of the commands is timed
        in Just (km, (desc, timed, undefined))
    kcMacroDesc = mapMaybe macro2cmd $ M.toAscList kmacro
  in toSlideshow True -- TODO: 80 below is a hack
    [ [T.justifyLeft 80 ' ' "Basic keys. [press SPACE to advance]"] ++ [blank]
      ++ mov ++ [moreMsg]
    , [T.justifyLeft 80 ' ' "Basic keys. [press SPACE to advance]"] ++ [blank]
      ++ [keyCaption] ++ keys kcMajor ++ major ++ [moreMsg]
    , [T.justifyLeft 80 ' ' "Basic keys."] ++ [blank]
      ++ [keyCaption] ++ keys (sort $ kcMinor ++ kcMacroDesc) ++ minor
    ]
