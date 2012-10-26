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
import qualified Game.LambdaHack.Key as K (Key, Modifier)

-- | Push a frame or a single frame's worth of delay to the frame queue.
displayFrame :: FrontendSession -> Bool -> Maybe Color.SingleFrame -> IO ()
displayFrame fs isRunning = display fs True isRunning

-- TODO: move promptGetKey here and then change its type to
-- promptGetKey :: FrontendSession
--              -> [((K.Key, K.Modifier), a)]
--              -> ((K.Key, K.Modifier) -> a)  -- ^ handle unexpected key
--              -> Color.SingleFrame
--              -> IO a
-- Then see if it can be used instead of the dangerous, low level nextEvent.
-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FrontendSession -> [(K.Key, K.Modifier)] -> Color.SingleFrame
             -> IO (K.Key, K.Modifier)
promptGetKey sess keys frame = do
  km <- promptGetAnyKey sess frame
  if null keys || km `elem` keys
    then return km
    else promptGetKey sess keys frame
