-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Frontend.Common
  ( RawFrontend(..)
  , startupBound, createRawFrontend, resetChanKey, saveKMP
  , modifierTranslate
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Concurrent
import qualified Control.Concurrent.STM as STM

import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Key (KMP (..))
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point

-- | Raw frontend definition. The minimal closed set of values that need
-- to depend on the specifics of the chosen frontend.
data RawFrontend = RawFrontend
  { fdisplay     :: SingleFrame -> IO ()
  , fshutdown    :: IO ()
  , fshowNow     :: MVar ()
  , fchanKey     :: STM.TQueue KMP
  , fprintScreen :: IO ()
  }

-- | Start up a frontend on a bound thread.
--
-- In fact, it is started on the very main thread, via a hack, because
-- apparently some SDL backends are not thread-safe
-- (<https://wiki.libsdl.org/FAQDevelopment>;
-- "this should only be run in the thread that initialized the video subsystem,
-- and for extra safety, you should consider only doing those things
-- on the main thread in any case")
-- and at least the newer OS X obtusely requires the main thread, see
-- https://github.com/AllureOfTheStars/Allure/issues/79
-- In case any other exotic architecture requires the main thread,
-- we make the hack the default for all (on frontends that require a bound
-- thread, e.g., SLD2 or GTK).
startupBound :: (MVar RawFrontend -> IO ()) -> IO RawFrontend
startupBound k = do
  rfMVar <- newEmptyMVar
  putMVar workaroundOnMainThreadMVar $ k rfMVar
  -- The following would run frontend on a bound thread, but it's not enough:
  -- a <- asyncBound $ k rfMVar
  -- link a
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
    , fprintScreen = return ()  -- dummy, except fro SDL2
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
