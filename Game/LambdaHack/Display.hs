-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Display
  ( -- * Re-exported frontend
    FrontendSession, startup, shutdown, frontendName, nextEvent, promptGetKey
    -- * Derived operations
  , displayFrame, displayLevel
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

import Data.Maybe

import Game.LambdaHack.Msg
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.State
import Game.LambdaHack.Perception
import Game.LambdaHack.Actor as Actor
import Game.LambdaHack.ActorState
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Draw

-- | Push a single frame to the frame queue or, in case of Nothing,
-- push a delay request equal to a single frame.
displayFrame :: FrontendSession -> Maybe Color.SingleFrame -> IO ()
displayFrame fs = display fs True False

-- | Display the screen, with an overlay, if any.
displayLevel :: FrontendSession -> ColorMode -> Kind.COps
             -> Perception -> State -> Overlay -> IO ()
displayLevel fs dm cops per s@State{splayer} overlay =
  let overlayFrame = draw dm cops per s overlay
      -- Speed up (by remving all empty frames) the show of the sequence
      -- of the move frames if the player is running.
      (_, Actor{bdir}, _) = findActorAnyLevel splayer s
      isRunning = isJust bdir
  in display fs True isRunning $ Just overlayFrame
