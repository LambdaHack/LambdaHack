-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Frontend.Chosen
  ( -- * Re-exported raw frontend
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
