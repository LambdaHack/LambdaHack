-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Action.Frontend.Chosen
  ( -- * Re-exported raw frontend
    FrontendSession, startup, frontendName, nextEvent, promptGetAnyKey, display
  ) where

-- Wrapper for selected Display frontend.

#ifdef CURSES
import Game.LambdaHack.Action.Frontend.Curses as D
#elif VTY
import Game.LambdaHack.Action.Frontend.Vty as D
#elif STD
import Game.LambdaHack.Action.Frontend.Std as D
#else
import Game.LambdaHack.Action.Frontend.Gtk as D
#endif
