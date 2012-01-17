module Game.LambdaHack.Keybinding
  ( Keybinding(..), handleKey, keyHelp, macroKey
  ) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.Dir

data Keybinding a = Keybinding
  { kmove  :: Dir -> a
  , krun   :: Dir -> a
  , kother :: M.Map K.Key (String, a)
  , kmacro :: M.Map K.Key K.Key
  , kmajor :: [K.Key]
  , ktimed :: [K.Key]
  }

handleKey :: X -> Keybinding a -> K.Key -> (String -> a) -> a
handleKey lxsize kb k abortWith=
  K.handleDirection lxsize k (kmove kb) $
    K.handleUDirection lxsize k (krun kb) $
      case M.lookup k (kother kb) of
        Just (_, c)  -> c
        Nothing -> abortWith $ "unknown command (" ++ K.showKey k ++ ")"

coImage :: M.Map K.Key K.Key -> K.Key -> [K.Key]
coImage kmacro k =
  let domain = M.keysSet kmacro
  in if k `S.member` domain
     then []
     else k : [ from | (from, to) <- M.assocs kmacro, to == k ]

keyHelp ::  Keybinding a -> [String]
keyHelp Keybinding{kother, kmacro, kmajor, ktimed} =
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
      , "Press space to see the next page, with the list of major commands."
      , ""
      ]
    majorKs   =
      [ ""
      , "Commands marked by * take hero time and are blocked on remote levels."
      , ""
      , "Press space to see the next page, with the list of minor commands."
      , ""
      ]
    minorKs   =
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
    mov     = map fmts movKs
    major   = map fmts majorKs
    minor   = map fmts minorKs
    keyCaption = fmt "keys" "command"
    disp k  = L.concatMap show $ coImage kmacro k
    ti k = if k `elem` ktimed then "*" else ""
    keys l  = [ fmt (disp k) (h ++ ti k) | (k, (h, _)) <- l, h /= "" ]
    keysAll = M.toAscList kother
    keysMajor = keys [ (k, c) | (k, c) <- keysAll, k `elem` kmajor]
    keysMinor = keys [ (k, c) | (k, c) <- keysAll, k `notElem` kmajor]
  in
    L.map unlines [ [blank] ++ mov
                  , [blank] ++ [keyCaption] ++ keysMajor ++ major
                  , [blank] ++ [keyCaption] ++ keysMinor ++ minor
                  ]

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
