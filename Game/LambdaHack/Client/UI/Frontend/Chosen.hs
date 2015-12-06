{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.Frontend.Chosen
  ( startup, frontendName
  ) where

#ifdef USE_CURSES
import Game.LambdaHack.Client.UI.Frontend.Curses
#elif USE_VTY
import Game.LambdaHack.Client.UI.Frontend.Vty
#elif (USE_BROWSER || USE_WEBKIT)
import Game.LambdaHack.Client.UI.Frontend.Dom
#else
import Game.LambdaHack.Client.UI.Frontend.Gtk
#endif
