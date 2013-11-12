{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Frontend.Chosen
  ( -- * Re-exported raw frontend
    FrontendSession, display, promptGetAnyKey, frontendName, startup, sdebugCli
  ) where

-- Wrapper for selected Display frontend.

#ifdef GTK
import Game.LambdaHack.Frontend.Gtk as D
#elif VTY
import Game.LambdaHack.Frontend.Vty as D
#elif CURSES
import Game.LambdaHack.Frontend.Curses as D
#elif STD
import Game.LambdaHack.Frontend.Std as D
#else
import Game.LambdaHack.Frontend.Gtk as D
#endif
