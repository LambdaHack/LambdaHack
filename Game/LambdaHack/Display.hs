-- | Display game data on the screen using one of the available frontends
-- and derived display operations.
module Game.LambdaHack.Display
  ( -- * Re-exported frontend
    FrontendSession, startup, frontendName, nextEvent, promptGetKey
    -- * Derived operations
  , displayFrame
  ) where

import Game.LambdaHack.Frontend
import qualified Game.LambdaHack.Color as Color

-- | Push a frame or a single frame's worth of delay to the frame queue.
displayFrame :: FrontendSession -> Bool -> Maybe Color.SingleFrame -> IO ()
displayFrame fs isRunning = display fs True isRunning
