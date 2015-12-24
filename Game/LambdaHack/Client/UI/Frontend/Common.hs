-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Frontend.Common
  ( RawFrontend(..)
  , startupAsync, startupBound, createRawFrontend, resetChanKey, saveKM
  , modifierTranslate
  ) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Monad (unless, void, when)
import Data.Maybe

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Overlay

data RawFrontend = RawFrontend
  { fdisplay  :: SingleFrame -> IO ()
  , fshutdown :: IO ()
  , fshowNow  :: !(MVar ())
  , fchanKey  :: !(STM.TQueue K.KM)
  }

startupAsync :: (MVar RawFrontend -> IO ()) -> IO RawFrontend
startupAsync k = do
  rfMVar <- newEmptyMVar
  a <- async $ k rfMVar
  link a
  takeMVar rfMVar

startupBound :: (MVar RawFrontend -> IO ()) -> IO RawFrontend
startupBound k = do
  rfMVar <- newEmptyMVar
  a <- asyncBound $ k rfMVar
  link a
  takeMVar rfMVar

createRawFrontend :: (SingleFrame -> IO ()) -> IO () -> IO RawFrontend
createRawFrontend fdisplay fshutdown = do
  -- Set up the channel for keyboard input.
  fchanKey <- STM.atomically STM.newTQueue
  -- Create the session record.
  fshowNow <- newMVar ()
  return $! RawFrontend
    { fdisplay
    , fshutdown
    , fshowNow
    , fchanKey
    }

-- | Empty the keyboard channel.
resetChanKey :: STM.TQueue K.KM -> IO ()
resetChanKey fchanKey = do
  res <- STM.atomically $ STM.tryReadTQueue fchanKey
  when (isJust res) $ resetChanKey fchanKey

saveKM :: RawFrontend -> K.KM -> IO ()
saveKM rf km = do
  unless (km == K.deadKM) $ do
    -- Store the key in the channel.
    STM.atomically $ STM.writeTQueue (fchanKey rf) km
    -- Instantly show any frame waiting for display.
    void $ tryPutMVar (fshowNow rf) ()

-- | Translates modifiers to our own encoding.
modifierTranslate :: Bool -> Bool -> Bool -> Bool -> K.Modifier
modifierTranslate modCtrl modShift modAlt modMeta
  | modCtrl = K.Control
  | modAlt || modMeta = K.Alt
  | modShift = K.Shift
  | otherwise = K.NoModifier
