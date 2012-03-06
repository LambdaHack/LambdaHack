-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Display
  ( -- * Re-exported frontend
    FrontendSession, startup, shutdown, frontendName
    -- * Derived operations
  , displayLevel, displayAnimation, displayNothing
  , nextEvent, promptGetKey
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

-- | Push a wait for a single frame to the frame queue.
displayNothing :: FrontendSession -> IO ()
displayNothing fs =
  display fs True False Nothing

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

-- | Display animations on top of the whole screen.
displayAnimation :: FrontendSession -> Kind.COps
                 -> Perception -> State -> Msg -> Color.Animation -> IO ()
displayAnimation fs cops per s topLineOnly anim =
  let basicFrame = draw ColorFull cops per s [topLineOnly]
      playAnimation am = display fs True False (Just  am)
  in mapM_ playAnimation $ animate s basicFrame anim
