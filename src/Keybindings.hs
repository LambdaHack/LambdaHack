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
    fmts s  = replicate 15 ' ' ++ s ++ replicate ((48 - length s) `max` 1) ' '
    blank   = fmt "" ""
    title   = fmt "keys" "command"
    footer  = fmts "(To search or open, bump into walls or doors. See PLAYING.md.)"
    disp k  = L.concatMap show $ aliases k
    rest    = [ fmt (disp k) h
              | (k, Described h _) <- M.toAscList (kother kb) ]
  in
    unlines ([blank, title] ++ rest ++ [blank, footer, blank])
