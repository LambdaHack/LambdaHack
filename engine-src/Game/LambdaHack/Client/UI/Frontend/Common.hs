-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Frontend.Common
  ( RawFrontend(..)
  , startupBound, createRawFrontend, resetChanKey, saveKMP, deliverKey
  , keysPending, modifierTranslate
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Concurrent.STM as STM

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.InputDecision
  (KeyDecision (..))
import           Game.LambdaHack.Client.UI.Key (KMP (..))
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.Misc

-- | Raw frontend definition. The minimal closed set of values that need
-- to depend on the specifics of the chosen frontend.
data RawFrontend = RawFrontend
  { fdisplay     :: SingleFrame -> IO ()
  , fshutdown    :: IO ()
  , fshowNow     :: MVar ()
  , fchanKey     :: STM.TQueue KMP
  , fprintScreen :: IO ()
  , fcoscreen    :: ScreenContent
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
-- thread, e.g., SLD2).
startupBound :: (MVar RawFrontend -> IO ()) -> IO RawFrontend
startupBound k = do
  rfMVar <- newEmptyMVar
  putMVar workaroundOnMainThreadMVar $ k rfMVar
  -- The following would run frontend on a bound thread, but it's not enough:
  -- a <- asyncBound $ k rfMVar
  -- link a
  takeMVar rfMVar

createRawFrontend :: ScreenContent -> (SingleFrame -> IO ()) -> IO ()
                  -> IO RawFrontend
createRawFrontend fcoscreen fdisplay fshutdown = do
  -- Set up the channel for keyboard input.
  fchanKey <- STM.atomically STM.newTQueue
  -- Create the session record.
  fshowNow <- newEmptyMVar
  return $! RawFrontend
    { fdisplay
    , fshutdown
    , fshowNow
    , fchanKey
    , fprintScreen = return ()  -- dummy, except for SDL2 and wasm
    , fcoscreen
    }

-- | Empty the keyboard channel.
resetChanKey :: STM.TQueue KMP -> IO ()
resetChanKey fchanKey = do
  res <- STM.atomically $ STM.tryReadTQueue fchanKey
  when (isJust res) $ resetChanKey fchanKey

saveKMP :: RawFrontend -> K.Modifier -> K.Key -> PointUI -> IO ()
saveKMP !rf !modifier !key !kmpPointer = do
  -- Instantly show any frame waiting for display.
  void $ tryTakeMVar $ fshowNow rf
  let kmp = KMP{kmpKeyMod = K.KM{..}, kmpPointer}
  unless (key == K.DeadKey) $
    -- Store the key in the channel.
    STM.atomically $ STM.writeTQueue (fchanKey rf) kmp

-- | Deliver a key as its 'KeyDecision' directs: empty the keys pending
-- before it, where the decision says so, and save it.
deliverKey :: RawFrontend -> KeyDecision -> K.Key -> PointUI -> IO ()
deliverKey rf KeyDecision{kdModifier, kdResetChan} key p = do
  when kdResetChan $ resetChanKey $ fchanKey rf
  saveKMP rf kdModifier key p

-- | Whether any key waits in the keyboard channel.
keysPending :: RawFrontend -> IO Bool
keysPending rf = do
  noKeysPending <- STM.atomically $ STM.isEmptyTQueue $ fchanKey rf
  return $! not noKeysPending

-- | Translates modifiers to our own encoding.
modifierTranslate :: Bool -> Bool -> Bool -> Bool -> K.Modifier
modifierTranslate modCtrl modShift modAlt modMeta
  | (modAlt || modMeta) && modShift = K.AltShift
  | modAlt || modMeta = K.Alt
  | modCtrl && modShift = K.ControlShift
  | modCtrl = K.Control
  | modShift = K.Shift
  | otherwise = K.NoModifier
