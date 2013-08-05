-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Frontend
  ( -- * Re-exported part of the raw frontend
    frontendName
    -- * Derived operation
  , startupF, getConfirmGeneric
    -- * Connection channels
  , ConnFrontend(..), ConnMulti(..)
  , connMulti, multiplex, unmultiplex, loopFrontend
  ) where

import Control.Concurrent
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Client.Animation (AcFrame (..), SingleFrame (..))
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Frontend.Chosen
import Game.LambdaHack.Utils.LQueue

-- | Connection channels between a client and a frontend.
data ConnFrontend = ConnFrontend
  { fromFrontend :: TQueue K.KM
  , toFrontend   :: TQueue (Either AcFrame ([K.KM], SingleFrame))
  }

instance Show ConnFrontend where
  show _ = "client-frontend connection channels"

type FromMulti = TQueue (FactionId, K.KM)

type ToMulti = TQueue (FactionId, Either AcFrame ([K.KM], SingleFrame))

-- | Multiplex connection channels, for the case of a frontend shared
-- among clients. This is transparent to the clients themselves.
data ConnMulti = ConnMulti
  { fromMulti :: FromMulti
  , toMulti   :: ToMulti
  }

startupF :: String -> IO () -> IO ()
startupF s k = startup s $ \fs -> do
  void $ forkIO $ loopFrontend fs connMulti
  k

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FrontendSession -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey sess keys frame = do
  km <- promptGetAnyKey sess frame
  if null keys || km `elem` keys
    then return km
    else promptGetKey sess keys frame

-- TODO: avoid unsafePerformIO; but server state is a wrong idea, too
connMulti :: ConnMulti
{-# NOINLINE connMulti #-}
connMulti = unsafePerformIO $ do
  fromMulti <- newTQueueIO
  toMulti <- newTQueueIO
  return ConnMulti{..}

multiplex :: TQueue (Either AcFrame ([K.KM], SingleFrame))
          -> FactionId -> ToMulti -> IO ()
multiplex toFrontend fid toMulti = loop
 where
  loop = do
    fr <- atomically $ STM.readTQueue toFrontend
    atomically $ STM.writeTQueue toMulti (fid, fr)
    loop

unmultiplex :: (FactionId -> ConnFrontend) -> FromMulti -> IO ()
unmultiplex fdict fromMulti = loop
 where
  loop = do
    (fid, km) <- atomically $ STM.readTQueue fromMulti
    let fromF = fromFrontend $ fdict fid
    atomically $ STM.writeTQueue fromF km
    loop

-- | Ignore unexpected kestrokes until a SPACE or ESC or others are pressed.
getConfirmGeneric :: Monad m
                  => ([K.KM] -> SingleFrame -> m K.KM)
                  -> [K.KM] -> SingleFrame -> m Bool
getConfirmGeneric pGetKey clearKeys frame = do
  let keys = [ K.KM {key=K.Space, modifier=K.NoModifier}
             , K.KM {key=K.Esc, modifier=K.NoModifier} ]
             ++ clearKeys
  km <- pGetKey keys frame
  case km of
    K.KM {key=K.Space, modifier=K.NoModifier} -> return True
    _ | km `elem` clearKeys -> return True
    _ -> return False

flushFrames :: FrontendSession -> FactionId
            -> EM.EnumMap FactionId (LQueue AcFrame)
            -> IO (EM.EnumMap FactionId (LQueue AcFrame))
flushFrames fs fid frMap = do
  let pGetKey keys frame = promptGetKey fs keys frame
      displayAc (AcConfirm fr) =
        void $ getConfirmGeneric pGetKey [] fr
      displayAc (AcRunning fr) =
        display fs True (Just fr)
      displayAc (AcNormal fr) =
        display fs False (Just fr)
      displayAc AcDelay =
        display fs False Nothing
      queue = fromMaybe newLQueue $ EM.lookup fid frMap
      frMap2 = EM.delete fid frMap
  mapM_ displayAc $ toListLQueue queue
  return frMap2

loopFrontend :: FrontendSession -> ConnMulti -> IO ()
loopFrontend fs ConnMulti{..} = loop (toEnum 1) EM.empty
 where
  insertFr :: FactionId -> AcFrame
           -> EM.EnumMap FactionId (LQueue AcFrame)
           -> EM.EnumMap FactionId (LQueue AcFrame)
  insertFr fid fr frMap =
    let queue = fromMaybe newLQueue $ EM.lookup fid frMap
    in EM.insert fid (writeLQueue queue fr) frMap
  loop :: FactionId -> EM.EnumMap FactionId (LQueue AcFrame) -> IO ()
  loop oldFid frMap = do
    (fid, efr) <- atomically $ STM.readTQueue toMulti
    case efr of
      Right (keys, frame) -> do
        frMap2 <- flushFrames fs fid frMap
        km <- promptGetKey fs keys frame
        atomically $ STM.writeTQueue fromMulti (fid, km)
        loop fid frMap2
      Left fr -> do
        let frMap2 = insertFr fid fr frMap
        loop fid frMap2
