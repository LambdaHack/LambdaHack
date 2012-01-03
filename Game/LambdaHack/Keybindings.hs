module Game.LambdaHack.Keybindings where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.Dir

data Described a = Described { chelp :: String, caction :: a }
                 | Undescribed { caction :: a }
type DirCommand a = Described (Dir -> a)

data Keybindings a = Keybindings
  { kdir   :: DirCommand a
  , kudir  :: DirCommand a
  , kother :: M.Map K.Key (Described a)
  , kmacro :: M.Map K.Key K.Key
  }

handleKey :: X -> Keybindings a -> K.Key -> (String -> a) -> a
handleKey lxsize kb k abortWith=
  K.handleDirection lxsize k (caction $ kdir kb) $
    K.handleUDirection lxsize k (caction $ kudir kb) $
      case M.lookup k (kother kb) of
        Just c  -> caction c
        Nothing -> abortWith $ "unknown command (" ++ K.showKey k ++ ")"

coImage :: M.Map K.Key K.Key -> K.Key -> [K.Key]
coImage kmacro k =
  let domain = M.keysSet kmacro
  in if k `S.member` domain
     then []
     else k : [ from | (from, to) <- M.assocs kmacro, to == k ]

-- TODO: mark commands that behave differently in targeting mode with "*"
keyHelp ::  Keybindings a -> String
keyHelp Keybindings{kother, kmacro} =
  let
    movKs   =
      [ "You move throughout the level using the numerical keypad or"
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
      , "In targeting mode, the keys move the target cursor."
      , ""
      , "To search, open or attack, bump into walls, doors or monsters."
      , ""
      , "For more playing instructions see file PLAYING.md."
      , ""
      , "Press space to see the next page."
      , ""
      , ""
      ]
    fmt k h = replicate 15 ' ' ++ k ++ replicate ((13 - length k) `max` 1) ' '
                               ++ h ++ replicate ((35 - length h) `max` 1) ' '
    fmts s  = replicate 2  ' ' ++ s ++ replicate ((71 - length s) `max` 1) ' '
    blank   = fmt "" ""
    mov     = map fmts movKs
    keyC    = fmt "keys" "command"
--    footer  =
--      fmts "(To search, open or attack, bump into walls, doors or monsters.)"
    disp k  = L.concatMap show $ coImage kmacro k
    rest    = [ fmt (disp k) h
              | (k, Described h _) <- M.toAscList kother ]
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
                           in if fromTr == toTr
                              then assert `failure` ("degenerate alias", toTr)
                              else (fromTr, toTr)
  in M.fromList $ L.map trMacro section
