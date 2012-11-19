-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Action.Frontend
  ( -- * Re-exported part of the raw frontend
    FrontendSession, startup, frontendName, nextEvent
    -- * Derived operations
  , displayFrame, promptGetKey
  ) where

import Game.LambdaHack.Action.Frontend.Chosen
import qualified Game.LambdaHack.Key as K (Key, Modifier)
import Game.LambdaHack.Animation (SingleFrame(..))

-- | Push a frame or a single frame's worth of delay to the frame queue.
displayFrame :: FrontendSession -> Bool -> Maybe SingleFrame -> IO ()
displayFrame fs isRunning = display fs True isRunning

-- TODO: move promptGetKey here and then change its type to
-- promptGetKey :: FrontendSession
--              -> [((K.Key, K.Modifier), a)]
--              -> ((K.Key, K.Modifier) -> a)  -- ^ handle unexpected key
--              -> SingleFrame
--              -> IO a
-- Then see if it can be used instead of the dangerous, low level nextEvent.
-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FrontendSession -> [(K.Key, K.Modifier)] -> SingleFrame
             -> IO (K.Key, K.Modifier)
promptGetKey sess keys frame = do
  km <- promptGetAnyKey sess frame
  if null keys || km `elem` keys
    then return km
    else promptGetKey sess keys frame
