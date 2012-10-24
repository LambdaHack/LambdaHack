-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Display
  ( -- * Re-exported frontend
    FrontendSession, startup, frontendName, nextEvent, promptGetKey
    -- * Derived operations
  , displayFrame
  ) where

-- Wrapper for selected Display frontend.

#ifdef CURSES
import Game.LambdaHack.Display.Curses as D
#elif VTY
import Game.LambdaHack.Display.Vty as D
#elif STD
import Game.LambdaHack.Display.Std as D
#else
import Game.LambdaHack.Display.Gtk as D
#endif

import qualified Game.LambdaHack.Color as Color

-- | Push a frame or a single frame's worth of delay to the frame queue.
displayFrame :: FrontendSession -> Bool -> Maybe Color.SingleFrame -> IO ()
displayFrame fs isRunning = display fs True isRunning
