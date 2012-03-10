-- | Generic binding of keys to commands, procesing macros,
-- printing command help. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Binding
  ( Binding(..), macroKey, keyHelp,
  ) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Msg

-- | Bindings and other information about player commands.
data Binding a = Binding
  { kcmd   :: M.Map (K.Key, K.Modifier) (String, Bool, a)
                                     -- ^ binding keys to commands
  , kmacro :: M.Map K.Key K.Key      -- ^ macro map
  , kmajor :: [K.Key]                -- ^ major, most often used, commands
  , kdir   :: [(K.Key, K.Modifier)]  -- ^ direction keys for moving and running
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
keyHelp :: Binding a -> [Overlay]
keyHelp Binding{kcmd, kmacro, kmajor} =
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
      , "Press keypad '5' or '.' to wait for one half-second step."
      , "In targeting mode the same keys move the targeting cursor."
      , ""
      , "Search, open and attack, by bumping into walls, doors and monsters."
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
      , "Press SPACE to clear the messages and go back to the game."
      ]
    fmt k h = replicate 16 ' ' ++ k ++ replicate ((15 - length k) `max` 1) ' '
                               ++ h ++ replicate ((41 - length h) `max` 1) ' '
    fmts s  = replicate 1  ' ' ++ s ++ replicate ((71 - length s) `max` 1) ' '
    blank   = fmt "" ""
    mov     = map fmts movBlurb
    major   = map fmts majorBlurb
    minor   = map fmts minorBlurb
    keyCaption = fmt "keys" "command"
    disp k  = L.concatMap show $ coImage kmacro k
    keys l  = [ fmt (disp k) (h ++ if timed then "*" else "")
              | ((k, _), (h, timed, _)) <- l, h /= "" ]
    (kcMajor, kcMinor) =
      L.partition ((`elem` kmajor) . fst . fst) (M.toAscList kcmd)
  in
    [ [blank] ++ mov
    , [blank] ++ [keyCaption] ++ keys kcMajor ++ major
    , [blank] ++ [keyCaption] ++ keys kcMinor ++ minor
    ]
