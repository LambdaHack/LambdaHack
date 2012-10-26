-- | Re-export the display operations of the chosen frontend
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Frontend
  ( -- * Re-exported frontend
    FrontendSession, startup, frontendName, nextEvent, promptGetAnyKey, display
  ) where

-- Wrapper for selected Display frontend.

#ifdef CURSES
import Game.LambdaHack.Frontend.Curses as D
#elif VTY
import Game.LambdaHack.Frontend.Vty as D
#elif STD
import Game.LambdaHack.Frontend.Std as D
#else
import Game.LambdaHack.Frontend.Gtk as D
#endif
