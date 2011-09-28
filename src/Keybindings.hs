module Keybindings where

import qualified Data.Map as M
import qualified Data.List as L

import Action
import Command
import qualified Keys as K

-- | Keybindings.
data Keybindings = Keybindings
  { kdir   :: DirCommand
  , kudir  :: DirCommand
  , kother :: M.Map K.Key Command
  }

handleKey :: Keybindings -> K.Key -> Action ()
handleKey kb k =
  K.handleDirection k (caction $ kdir kb) $
    K.handleUDirection k (caction $ kudir kb) $
      case M.lookup k (kother kb) of
        Just c  -> caction c
        Nothing -> abortWith $ "unknown command (" ++ K.showKey k ++ ")"

keyHelp :: (K.Key -> [K.Key]) -> Keybindings -> String
keyHelp aliases kb =
  let
    fmt k h = replicate 15 ' ' ++ k ++ replicate ((13 - length k) `max` 1) ' '
                               ++ h ++ replicate ((35 - length h) `max` 1) ' '
    fmts s  = replicate 2  ' ' ++ s ++ replicate ((71 - length s) `max` 1) ' '
    blank   = fmt "" ""
    movKs   =
      [ ""
      , "You move throughout the level using the numerical keypad or"
      , "the vi text editor keys (also known as \"Rogue-like keys\")."
      , ""
      , "               7 8 9     y k u"
      , "                \\|/       \\|/"
      , "               4-5-6     h-.-l"
      , "                /|\\       /|\\"
      , "               1 2 3     b j n"
      , ""
      , "Shift and a movement key make the hero run in the indicated direction,"
      , "until anything of interest is spotted. '5' and '.' skip a turn."
      , ""
      , "To search, open or attack, bump into walls, doors or monsters."
      , ""
      , "For more playing instructions see file PLAYING.md."
      , ""
      , "Press space to see the next page."
      , ""
      , ""
      ]
    mov     = map fmts movKs
    keyC    = fmt "keys" "command"
--    footer  =
--      fmts "(To search, open or attack, bump into walls, doors or monsters.)"
    disp k  = L.concatMap show $ aliases k
    rest    = [ fmt (disp k) h
              | (k, Described h _) <- M.toAscList (kother kb) ]
  in
    unlines ([blank] ++ mov ++ [blank, keyC] ++ rest)
--             [blank, footer, blank])
