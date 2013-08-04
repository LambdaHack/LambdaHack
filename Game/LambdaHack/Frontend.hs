-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Frontend
  ( -- * Re-exported part of the raw frontend
    FrontendSession, startup, frontendName, display
    -- * Derived operation
  , promptGetKey
    -- * Connection channels
  , FrontendConn (..)
  ) where

import Control.Concurrent.STM.TQueue
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.Animation (AcFrame (..), SingleFrame (..))
import qualified Game.LambdaHack.Client.Key as K (KM)
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Frontend.Chosen

-- | Connection channels between a client and a frontend.
data FrontendConn = FrontendConn
  { ftoClient   :: TQueue K.KM
  , ftoFrontend :: TQueue AcFrame
  }

instance Show FrontendConn where
  show _ = "client-frontend connection channels"

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FrontendSession -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey sess keys frame = do
  km <- promptGetAnyKey sess frame
  if null keys || km `elem` keys
    then return km
    else promptGetKey sess keys frame
