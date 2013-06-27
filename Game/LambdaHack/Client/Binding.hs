{-# LANGUAGE OverloadedStrings #-}
-- | Generic binding of keys to commands, procesing macros,
-- printing command help. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.Binding
  ( Binding(..), stdBinding, keyHelp,
  ) where

import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.HumanCmd
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Common.Msg

-- | Bindings and other information about human player commands.
data Binding = Binding
  { kcmd    :: M.Map K.KM (Text, Bool, HumanCmd)
                                    -- ^ binding keys to commands
  , kmacro  :: M.Map K.KM K.KM      -- ^ macro map
  , kmajor  :: [K.KM]               -- ^ major commands
  , kminor  :: [K.KM]               -- ^ minor commands
  , krevMap :: M.Map HumanCmd K.KM  -- ^ from cmds to their main keys
  }

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: ConfigUI  -- ^ game config
           -> Binding   -- ^ concrete binding
stdBinding !config@ConfigUI{configMacros} =
  let kmacro = M.fromList $ configMacros
      heroSelect k = ( K.KM { key=K.Char (Char.intToDigit k)
                            , modifier=K.NoModifier }
                     , SelectHero k )
      cmdList =
        configCommands config
        ++ K.moveBinding Move Run
        ++ fmap heroSelect [0..9]
      mkDescribed cmd = (cmdDescription cmd, noRemoteHumanCmd cmd, cmd)
      mkCommand (km, def) = (km, mkDescribed def)
      semList = L.map mkCommand cmdList
  in Binding
  { kcmd = M.fromList semList
  , kmacro
  , kmajor = L.map fst $ L.filter (majorHumanCmd . snd) cmdList
  , kminor = L.map fst $ L.filter (minorHumanCmd . snd) cmdList
  , krevMap = M.fromList $ map swap cmdList
  }

coImage :: M.Map K.KM K.KM -> K.KM -> [K.KM]
coImage kmacro k =
  let domain = M.keysSet kmacro
  in if k `S.member` domain
     then []
     else k : [ from | (from, to) <- M.assocs kmacro, to == k ]

-- | Produce a set of help screens from the key bindings.
keyHelp :: Binding -> Slideshow
keyHelp Binding{kcmd, kmacro, kmajor, kminor} =
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
    fmt k h = T.replicate 16 " "
              <> T.justifyLeft 15 ' ' k
              <> T.justifyLeft 41 ' ' h
    fmts s  = " " <> T.justifyLeft 71 ' ' s
    blank   = fmt "" ""
    mov     = map fmts movBlurb
    major   = map fmts majorBlurb
    minor   = map fmts minorBlurb
    keyCaption = fmt "keys" "command"
    disp k  = T.concat $ map K.showKM $ coImage kmacro k
    keys l  = [ fmt (disp k) (h <> if timed then "*" else "")
              | (k, (h, timed, _)) <- l, h /= "" ]
    (kcMajor, kcRest) =
      L.partition ((`elem` kmajor) . fst) (M.toAscList kcmd)
    (kcMinor, _) =
      L.partition ((`elem` kminor) . fst) kcRest
  in toSlideshow $
    [ ["Basic keys. [press SPACE to advance]"] ++ [blank]
      ++ mov ++ [moreMsg]
    , ["Basic keys. [press SPACE to advance]"] ++ [blank]
      ++ [keyCaption] ++ keys kcMajor ++ major ++ [moreMsg]
    , ["Basic keys."] ++ [blank]
      ++ [keyCaption] ++ keys kcMinor ++ minor
    ]
