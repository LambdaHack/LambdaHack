-- | Binding of keys to commands, macros, command help.
module Game.LambdaHack.Keybinding
  ( Keybinding(..), macroKey, keyHelp,
  ) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import qualified Game.LambdaHack.Keys as K

-- | Bindings and other information about player commands.
data Keybinding a = Keybinding
  { kcmd   :: M.Map K.Key (String, a)  -- ^ binding to descriptions and cmds
  , kmacro :: M.Map K.Key K.Key        -- ^ macros map
  , kmajor :: [K.Key]  -- ^ major, most often used, commands
  , ktimed :: [K.Key]  -- ^ commands that take time, except movement commands
  }

-- | Produce the macro map from a macro association list
-- taken from the config file. Macros cannot depend on each other.
-- The map is fully evaluated to catch errors in macro definitions early.
macroKey :: [(String, String)] -> M.Map K.Key K.Key
macroKey section =
  let trans k = case K.keyTranslate k of
                  K.Unknown s -> assert `failure` ("unknown macro key " ++ s)
                  kt -> kt
      trMacro (from, to) = let !fromTr = trans from
                               !toTr  = trans to
                           in if fromTr == toTr
                              then assert `failure` ("degenerate alias", toTr)
                              else (fromTr, toTr)
  in M.fromList $ L.map trMacro section

coImage :: M.Map K.Key K.Key -> K.Key -> [K.Key]
coImage kmacro k =
  let domain = M.keysSet kmacro
  in if k `S.member` domain
     then []
     else k : [ from | (from, to) <- M.assocs kmacro, to == k ]

-- | Produce a set of help screens from the key bindings.
keyHelp :: Keybinding a -> [String]
keyHelp Keybinding{kcmd, kmacro, kmajor, ktimed} =
  let
    movBlurb =
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
      , "Press space to see the next page, with the list of major commands."
      , ""
      ]
    majorBlurb =
      [ ""
      , "Commands marked by * take hero time and are blocked on remote levels."
      , ""
      , "Press space to see the next page, with the list of minor commands."
      , ""
      ]
    minorBlurb =
      [ ""
      , "For more playing instructions see file PLAYING.md."
      , ""
      , "Press space to go back to the game."
      , ""
      ]
    fmt k h = replicate 15 ' ' ++ k ++ replicate ((13 - length k) `max` 1) ' '
                               ++ h ++ replicate ((35 - length h) `max` 1) ' '
    fmts s  = replicate 2  ' ' ++ s ++ replicate ((71 - length s) `max` 1) ' '
    blank   = fmt "" ""
    mov     = map fmts movBlurb
    major   = map fmts majorBlurb
    minor   = map fmts minorBlurb
    keyCaption = fmt "keys" "command"
    disp k  = L.concatMap show $ coImage kmacro k
    ti k    = if k `elem` ktimed then "*" else ""
    keys l  = [ fmt (disp k) (h ++ ti k) | (k, (h, _)) <- l, h /= "" ]
    (kcMajor, kcMinor) = L.partition ((`elem` kmajor) . fst) (M.toAscList kcmd)
  in
    L.map unlines [ [blank] ++ mov
                  , [blank] ++ [keyCaption] ++ keys kcMajor ++ major
                  , [blank] ++ [keyCaption] ++ keys kcMinor ++ minor
                  ]
