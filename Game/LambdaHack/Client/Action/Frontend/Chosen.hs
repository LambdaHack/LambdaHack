{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.Action.Frontend.Chosen
  ( -- * Re-exported raw frontend
    FrontendSession, startup, frontendName, nextEvent, promptGetAnyKey, display
  ) where

-- Wrapper for selected Display frontend.

#ifdef GTK
import Game.LambdaHack.Client.Action.Frontend.Gtk as D
#elif VTY
import Game.LambdaHack.Client.Action.Frontend.Vty as D
#elif CURSES
import Game.LambdaHack.Client.Action.Frontend.Curses as D
#elif STD
import Game.LambdaHack.Client.Action.Frontend.Std as D
#else
import Game.LambdaHack.Client.Action.Frontend.Gtk as D
#endif
