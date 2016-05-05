-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Frontend.Common
  ( RawFrontend(..), KMP(..)
  , startupAsync, startupBound, createRawFrontend, resetChanKey
  , saveKMP, saveDblKMP
  , modifierTranslate
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Point

data KMP = KMP { kmpKeyMod  :: !K.KM
               , kmpPointer :: !Point }

data RawFrontend = RawFrontend
  { fdisplay  :: SingleFrame -> IO ()
  , fshutdown :: IO ()
  , fshowNow  :: !(MVar ())
  , fchanKey  :: !(STM.TQueue KMP)
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
  fshowNow <- newEmptyMVar
  return $! RawFrontend
    { fdisplay
    , fshutdown
    , fshowNow
    , fchanKey
    }

-- | Empty the keyboard channel.
resetChanKey :: STM.TQueue KMP -> IO ()
resetChanKey fchanKey = do
  res <- STM.atomically $ STM.tryReadTQueue fchanKey
  when (isJust res) $ resetChanKey fchanKey

saveKMP :: RawFrontend -> K.Modifier -> K.Key -> Point -> IO ()
saveKMP rf modifier key kmpPointer = do
  -- Instantly show any frame waiting for display.
  void $ tryTakeMVar $ fshowNow rf
  let kmp = KMP{kmpKeyMod = K.KM{..}, kmpPointer}
  unless (key == K.DeadKey) $
    -- Store the key in the channel.
    STM.atomically $ STM.writeTQueue (fchanKey rf) kmp

-- From Web standards:
-- "The click event MAY also be followed by the dblclick event"
-- "The click event MAY be preceded by the mousedown and mouseup
-- events on the same element"
-- Gtk behaves similarly. We handle the problem here.
saveDblKMP :: RawFrontend -> K.Modifier -> K.Key -> Point -> IO ()
saveDblKMP rf modifier key kmpPointer = do
  -- Instantly show any frame waiting for display.
  void $ tryTakeMVar $ fshowNow rf
  let kmp = KMP{kmpKeyMod = K.KM{..}, kmpPointer}
  unless (key == K.DeadKey) $ case key of
    K.LeftDblClick -> STM.atomically $ do
      -- Remove the mouseDown events that are a part of the double click.
      mleftButton <- STM.tryReadTQueue (fchanKey rf)
      case mleftButton of
        Just KMP{kmpKeyMod = K.KM{key=K.LeftButtonPress}} -> do
          mleftButton2 <- STM.tryReadTQueue (fchanKey rf)
          case mleftButton2 of
            Just KMP{kmpKeyMod = K.KM{key=K.LeftButtonPress}} ->
              -- Store the key in the channel.
              STM.writeTQueue (fchanKey rf) kmp
            _ -> return ()  -- too long dblclick delay, a click already consumed
        _ -> return ()  -- both clicks already consumed; separately
    _ -> STM.atomically $ STM.writeTQueue (fchanKey rf) kmp

-- | Translates modifiers to our own encoding.
modifierTranslate :: Bool -> Bool -> Bool -> Bool -> K.Modifier
modifierTranslate modCtrl modShift modAlt modMeta
  | modCtrl = K.Control
  | modAlt || modMeta = K.Alt
  | modShift = K.Shift
  | otherwise = K.NoModifier
