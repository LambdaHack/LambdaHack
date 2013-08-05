-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Frontend
  ( -- * Re-exported part of the raw frontend
    FrontendSession, startup, frontendName, display
    -- * Derived operation
  , promptGetKey
    -- * Connection channels
  , ConnFrontend (..), multiFrontendTQueue, multiplex, loopFrontend
  ) where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.EnumMap.Strict as EM
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Client.Animation (AcFrame (..), SingleFrame (..))
import qualified Game.LambdaHack.Client.Key as K (KM)
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Frontend.Chosen
import Game.LambdaHack.Utils.LQueue

-- | Connection channels between a client and a frontend.
data ConnFrontend = ConnFrontend
  { fromFrontend :: TQueue K.KM
  , toFrontend   :: TQueue AcFrame
  }

instance Show ConnFrontend where
  show _ = "client-frontend connection channels"

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FrontendSession -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey sess keys frame = do
  km <- promptGetAnyKey sess frame
  if null keys || km `elem` keys
    then return km
    else promptGetKey sess keys frame

-- TODO: avoid unsafePerformIO; but server state is a wrong idea, too
multiFrontendTQueue :: TQueue (FactionId, AcFrame)
{-# NOINLINE multiFrontendTQueue #-}
multiFrontendTQueue = unsafePerformIO newTQueueIO

multiplex :: TQueue AcFrame -> FactionId -> TQueue (FactionId, AcFrame)
          -> IO ()
multiplex toFrontend side multiFrontend = loop
 where
  loop = do
    fr <- atomically $ STM.readTQueue toFrontend
    atomically $ STM.writeTQueue multiFrontend (side, fr)
    loop

loopFrontend :: TQueue (FactionId, AcFrame) -> IO ()
loopFrontend multiFrontend = loop EM.empty
 where
  insertFr :: FactionId -> AcFrame
           -> EM.EnumMap FactionId (LQueue AcFrame)
           -> EM.EnumMap FactionId (LQueue AcFrame)
  insertFr side fr frMap =
    let queue = fromMaybe newLQueue $ EM.lookup side frMap
    in EM.insert side (writeLQueue queue fr) frMap
  loop :: EM.EnumMap FactionId (LQueue AcFrame) -> IO ()
  loop frMap = do
    (side, fr) <- atomically $ STM.readTQueue multiFrontend
    let frMap2 = insertFr side fr frMap
    -- TODO: display something
    loop frMap2
