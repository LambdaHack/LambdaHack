{-# LANGUAGE OverloadedStrings #-}
-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Frontend
  ( -- * Re-exported part of the raw frontend
    frontendName
    -- * Derived operation
  , startupF, getConfirmGeneric
    -- * Connection channels
  , ChanFrontend, ConnMulti(..), connMulti, loopFrontend
  ) where

import Control.Concurrent
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Client.Animation
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Frontend.Chosen
import Game.LambdaHack.Utils.LQueue

type ChanFrontend = TQueue K.KM

type FromMulti = MVar (Int, FactionId -> ChanFrontend)

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
  fromMulti <- newMVar undefined
  toMulti <- newTQueueIO
  return ConnMulti{..}

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
  let queue = toListLQueue $ fromMaybe newLQueue $ EM.lookup fid frMap
      frMap2 = EM.delete fid frMap
  mapM_ (displayAc fs) queue
  return frMap2

displayAc :: FrontendSession -> AcFrame -> IO ()
displayAc fs (AcConfirm fr) = void $ getConfirmGeneric (promptGetKey fs) [] fr
displayAc fs (AcRunning fr) = display fs True (Just fr)
displayAc fs (AcNormal fr) = display fs False (Just fr)
displayAc fs AcDelay = display fs False Nothing

getSingleFrame :: AcFrame -> Maybe (Bool, SingleFrame)
getSingleFrame (AcConfirm fr) = Just (False, fr)
getSingleFrame (AcRunning fr) = Just (True, fr)
getSingleFrame (AcNormal fr) = Just (False, fr)
getSingleFrame AcDelay = Nothing

toSingles :: FactionId -> EM.EnumMap FactionId (LQueue AcFrame)
          -> [(Bool, SingleFrame)]
toSingles fid frMap =
  let queue = toListLQueue $ fromMaybe newLQueue $ EM.lookup fid frMap
  in mapMaybe getSingleFrame queue

fadeF :: FrontendSession -> Bool -> FactionId -> SingleFrame -> IO ()
fadeF fs out side frame = do
  let topRight = True
      lxsize = xsizeSingleFrame frame
      lysize = ysizeSingleFrame frame
      msg = "Player" <+> showT (fromEnum side) <> ", get ready!"
  animMap <- rndToIO $ fadeout out topRight lxsize lysize
  let sfTop = truncateMsg lxsize msg
      basicFrame = frame {sfTop}
      animFrs = renderAnim lxsize lysize basicFrame animMap
      frs | out = animFrs
            -- Empty frame to mark the fade-in end,
            -- to trim only to here if SPACE pressed.
          | otherwise = animFrs ++ [Nothing]
  mapM_ (display fs False) frs

insertFr :: FactionId -> AcFrame
         -> EM.EnumMap FactionId (LQueue AcFrame)
         -> EM.EnumMap FactionId (LQueue AcFrame)
insertFr fid fr frMap =
  let queue = fromMaybe newLQueue $ EM.lookup fid frMap
  in EM.insert fid (writeLQueue queue fr) frMap

-- TODO: save or display all at game save
loopFrontend :: FrontendSession -> ConnMulti -> IO ()
loopFrontend fs ConnMulti{..} = loop Nothing EM.empty
 where
  loop :: Maybe (FactionId, SingleFrame)
       -> EM.EnumMap FactionId (LQueue AcFrame)
       -> IO ()
  loop oldFidFrame frMap = do
    (fid, efr) <- atomically $ STM.readTQueue toMulti
    case efr of
      Right (keys, frame) -> do
        frMap2 <-
          if Just fid == fmap fst oldFidFrame then
            return frMap
          else do
            (nH, _) <- readMVar fromMulti
            frMap2 <- case oldFidFrame of
              Nothing -> return frMap
              Just (oldFid, oldFrame) -> do
                frMap2 <- flushFrames fs oldFid frMap
                let singles = toSingles oldFid frMap  -- not @frMap2@!
                    (_, lastFrame) = fromMaybe (undefined, oldFrame)
                                     $ listToMaybe $ reverse singles
                    (running, _) = fromMaybe (False, undefined)
                                   $ listToMaybe singles
                unless (running || nH < 2) $ fadeF fs True fid lastFrame
                return frMap2
            let singles = toSingles fid frMap2
                (running, firstFrame) = fromMaybe (False, frame)
                                        $ listToMaybe singles
            unless (running || nH < 2) $ fadeF fs False fid firstFrame
            return frMap2
        frMap3 <- flushFrames fs fid frMap2
        km <- promptGetKey fs keys frame
        (nH, fdict) <- takeMVar fromMulti
        let chanFrontend = fdict fid
        atomically $ STM.writeTQueue chanFrontend km
        putMVar fromMulti (nH, fdict)
        loop (Just (fid, frame)) frMap3
      Left fr | Just (oldFid, oldFrame) <- oldFidFrame, fid == oldFid -> do
        displayAc fs fr
        let frame = maybe oldFrame snd $ getSingleFrame fr
        loop (Just (fid, frame)) frMap
      Left fr -> do
        let frMap2 = insertFr fid fr frMap
        loop oldFidFrame frMap2
