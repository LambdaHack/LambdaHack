-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Client.Action.Frontend
  ( -- * Re-exported part of the raw frontend
    FrontendSession, startup, frontendName, display
    -- * Derived operation
  , promptGetKey
  ) where

import Game.LambdaHack.Client.Action.Frontend.Chosen
import Game.LambdaHack.Client.Animation (SingleFrame (..))
import qualified Game.LambdaHack.Client.Key as K (KM)

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FrontendSession -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey sess keys frame = do
  km <- promptGetAnyKey sess frame
  if null keys || km `elem` keys
    then return km
    else promptGetKey sess keys frame
