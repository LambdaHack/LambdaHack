-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Frontend.Common
  ( RawFrontend(..), KMP(..)
  , startupBound, createRawFrontend, resetChanKey, saveKMP
  , modifierTranslate
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Common.Point

data KMP = KMP { kmpKeyMod  :: !K.KM
               , kmpPointer :: !Point }

data RawFrontend = RawFrontend
  { fdisplay  :: !(SingleFrame -> IO ())
  , fshutdown :: !(IO ())
  , fshowNow  :: !(MVar ())
  , fchanKey  :: !(STM.TQueue KMP)
  }

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
saveKMP !rf !modifier !key !kmpPointer = do
  -- Instantly show any frame waiting for display.
  void $ tryTakeMVar $ fshowNow rf
  let kmp = KMP{kmpKeyMod = K.KM{..}, kmpPointer}
  unless (key == K.DeadKey) $
    -- Store the key in the channel.
    STM.atomically $ STM.writeTQueue (fchanKey rf) kmp

-- | Translates modifiers to our own encoding.
modifierTranslate :: Bool -> Bool -> Bool -> Bool -> K.Modifier
modifierTranslate modCtrl modShift modAlt modMeta
  | modCtrl = K.Control
  | modAlt || modMeta = K.Alt
  | modShift = K.Shift
  | otherwise = K.NoModifier
