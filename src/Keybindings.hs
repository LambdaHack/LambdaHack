module Keybindings where

import qualified Data.Map as M
import qualified Data.List as L

import Utils.Assert
import Action
import Command
import Geometry
import qualified Keys as K

-- | Keybindings.
data Keybindings = Keybindings
  { kdir   :: DirCommand
  , kudir  :: DirCommand
  , kother :: M.Map K.Key Command
  }

handleKey :: X -> Keybindings -> K.Key -> Action ()
handleKey lxsize kb k =
  K.handleDirection lxsize k (caction $ kdir kb) $
    K.handleUDirection lxsize k (caction $ kudir kb) $
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

-- | Maps a key to the canonical key for the command it denotes.
-- Takes into account the keypad and any macros from a config file.
-- Macros cannot depend on each other, but they can on canonMoveKey.
-- This has to be fully evaluated to catch errors in macro definitions early.
macroKey :: [(String, String)] -> M.Map K.Key K.Key
macroKey section =
  let trans k = case K.keyTranslate k of
                  K.Unknown s -> assert `failure` ("unknown macro key " ++ s)
                  kt -> kt
      trMacro (from, to) = let fromTr = trans from
                               !toTr  = K.canonMoveKey $ trans to
                           in  if fromTr == toTr
                               then assert `failure` ("degenerate alias", toTr)
                               else (fromTr, toTr)
  in  M.fromList $ L.map trMacro section
