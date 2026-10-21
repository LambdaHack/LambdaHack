{-# LANGUAGE MagicHash #-}
-- | Text frontend running in a browser via the wasm32-wasi backend, driven by
-- a thin TypeScript terminal-emulator over the GHC wasm JSFFI.
--
-- Display: each frame's raw @Word32@ cell array (the AttrCharW32 encoding:
-- @char = w >> 16@, @fg = (w >> 8) & 0xFF@, @bg = w & 0xFF@) is handed to JS by
-- address into wasm linear memory; the TS side decodes and paints. Input: JS
-- calls the exported @lhKey@ with each keydown event in the terminal, and
-- @lhKey@ decides the browser's default action and translates and queues
-- the key.
module Game.LambdaHack.Client.UI.Frontend.Wasm
  (
#ifdef USE_WASM
   startup, frontendName
#endif
  ) where

#ifdef USE_WASM
import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Exception as Ex
import           Data.IORef
import           Data.Primitive.ByteArray (ByteArray (..), ByteArray#)
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as VU
import           Data.Word (Word32)
import           Foreign.Storable (sizeOf)
import           GHC.Wasm.Prim (JSString (..), JSVal, fromJSString)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import           Game.LambdaHack.Client.UI.Frontend.InputDecision
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Submit a frame: @(address, width, height)@ of a @Word32@ cell buffer in
-- wasm linear memory. The JS side (set up by the loader
-- as @globalThis.lhSubmitFrame@) snapshots the buffer and schedules a
-- render; it does not paint synchronously.
-- @unsafe@: synchronous, so no GC moves the buffer mid-call.
foreign import javascript unsafe "globalThis.lhSubmitFrame($1 + $2, $3, $4)"
  js_submitFrame :: ByteArray# -> Int -> Int -> Int -> IO ()

word32BackingArray :: U.Vector Word32 -> (ByteArray# -> Int -> IO a) -> IO a
word32BackingArray (VU.V_Word32 (VP.Vector off _len (ByteArray ba#))) k =
  k ba# (off * sizeOf (undefined :: Word32))

-- | The active frontend, so the exported 'lhKey' can reach its key queue.
-- Must be global: @foreign export javascript@ functions are static
-- top-level bindings, not closures, so 'lhKey' has no way to directly
-- reference the @rf@ value 'startup' constructs -- there is no shared
-- scope between them, and JS calls 'lhKey' independently, later, with no
-- handle to the frontend to pass in. A persistent, module-level reference
-- is the only way for the two to communicate. Only one frontend exists per
-- wasm instance, hence 'Maybe' rather than a collection.
{-# NOINLINE rfRef #-}
rfRef :: IORef (Maybe RawFrontend)
rfRef = unsafePerformIO $ newIORef Nothing

-- | The name of the frontend.
frontendName :: String
frontendName = "wasm"

-- | Set up the frontend input and output.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen _ = do
  rf <- createRawFrontend coscreen (display coscreen) shutdown
  writeIORef rfRef $ Just rf
  return rf

shutdown :: IO ()
shutdown = return () -- nothing to clean up

-- | Output to the screen via the frontend.
display :: ScreenContent -> SingleFrame -> IO ()
display ScreenContent{rwidth, rheight} SingleFrame{singleArray} =
  let v = PointArray.avector singleArray :: U.Vector Word32
  in word32BackingArray v $ \ba# byteOffset ->
       js_submitFrame ba# byteOffset rwidth rheight

-- | Handle a keydown delivered from JS: the @KeyboardEvent@ itself, read
-- for its key and modifiers, and taken from the browser unless 'decideKey'
-- leaves it a passthrough. A sync export, so that the decision lands before
-- the keydown dispatch returns, and so built on 'syncExport'.
lhKey :: JSVal -> IO ()
lhKey e = syncExport "lhKey" $ do
  jsKey <- js_key e
  modCtrl <- js_ctrlKey e
  modShift <- js_shiftKey e
  modAlt <- js_altKey e
  modMeta <- js_metaKey e
  let modifier = modifierTranslate modCtrl modShift modAlt modMeta
      key = K.keyTranslateWeb (fromJSString jsKey) (modifier == K.Shift)
      KeyDecision{kdModifier, kdResetChan, kdPreventBrowser} =
        decideKey modifier key
  when kdPreventBrowser $ js_preventDefaultAndStop e
  mrf <- readIORef rfRef
  forM_ mrf $ \rf -> do
    when kdResetChan $ resetChanKey (fchanKey rf)
    saveKMP rf kdModifier key (PointUI 0 0)

foreign export javascript "lhKey sync"
  lhKey :: JSVal -> IO ()

-- | Run the body of a sync JSFFI export, and do the two things such
-- an export does not do for itself. Every sync export is built on this,
-- which @tools/check-sync-exports.py@ enforces; an async export needs
-- neither. A sync export passes no exception to its JS caller, so one is
-- caught and reported here. Nor does it run the threads it wakes: GHC runs
-- it as it runs a C FFI export, through @rts_inCall@, whose scheduler
-- returns once the export's own thread finishes, so a thread the body woke
-- stays in the run queue. In a reactor nothing runs that thread until JS
-- calls in again, and four kinds of call run the queue until it is empty:
-- an async export, a settled promise of an async import, a @throwTo@
-- on an async export's promise, and the @setImmediate@ the scheduler loop
-- requests when it stops after 15 ms. An idle game has none of them
-- due, so the key 'lhKey' queues would wait for an unrelated event. Hence
-- 'rts_scheduleWork' after the body, unconditionally, since the loop it
-- schedules returns at once from an empty run queue. A @yield@ in its place
-- is not enough, and did not suffice for 'lhKey': it runs the threads
-- the export woke, but a thread one of those wakes goes to the end
-- of the queue, behind the export's own, which then finishes and leaves
-- the scheduler. GHC's JS backend needs none of this, its wakeup itself
-- starting the main loop. The gap is GHC's, reported at
-- https://gitlab.haskell.org/ghc/ghc/-/work_items/28010; once every GHC
-- the wasm build is made with carries a fix, the 'rts_scheduleWork' call
-- goes.
syncExport :: String -> IO () -> IO ()
syncExport name body = do
  Ex.handle reportException body
  rts_scheduleWork
 where
  reportException :: Ex.SomeException -> IO ()
  reportException ex =
    hPutStrLn stderr $ name ++ ": " ++ Ex.displayException ex

-- | Run the RTS scheduler loop once the current JavaScript task is done,
-- through the @setImmediate@ the post-linked JSFFI glue provides. No public
-- RTS header declares it, so a GHC may change or drop it unannounced.
foreign import ccall unsafe "rts_scheduleWork"
  rts_scheduleWork :: IO ()

-- | The fields of a @KeyboardEvent@ 'lhKey' reads. @unsafe@: synchronous,
-- as the sync export they serve must be.
foreign import javascript unsafe "$1.key"
  js_key :: JSVal -> IO JSString

foreign import javascript unsafe "$1.ctrlKey"
  js_ctrlKey :: JSVal -> IO Bool

foreign import javascript unsafe "$1.shiftKey"
  js_shiftKey :: JSVal -> IO Bool

-- | AltGraph counts as Alt, as in Dom.hs.
foreign import javascript unsafe "$1.altKey || $1.getModifierState('AltGraph')"
  js_altKey :: JSVal -> IO Bool

foreign import javascript unsafe "$1.metaKey"
  js_metaKey :: JSVal -> IO Bool

-- | Take the event from the browser; Dom.hs never calls one of these
-- without the other.
foreign import javascript unsafe "($1.preventDefault(), $1.stopPropagation())"
  js_preventDefaultAndStop :: JSVal -> IO ()

-- | Handle a wheel event delivered from JS over a given screen cell
-- (0-based column/row).
lhWheel :: Int -> Int -> Double -> Bool -> Bool -> Bool -> Bool -> IO ()
lhWheel col row deltaY modCtrl modShift modAlt modMeta = do
  mrf <- readIORef rfRef
  forM_ mrf $ \rf -> do
    let modifier = modifierTranslate modCtrl modShift modAlt modMeta
        pUI = squareToUI $ PointSquare col row
    forM_ (decideWheel deltaY) $ \key -> saveKMP rf modifier key pUI

foreign export javascript "lhWheel"
  lhWheel :: Int -> Int -> Double -> Bool -> Bool -> Bool -> Bool -> IO ()

-- | Handle a mouseup delivered from JS over a given screen cell (0-based
-- column/row) with the DOM @MouseEvent.button@ code.
lhMouseUp :: Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> IO ()
lhMouseUp col row button modCtrl modShift modAlt modMeta = do
  mrf <- readIORef rfRef
  forM_ mrf $ \rf -> do
    let modifier = modifierTranslate modCtrl modShift modAlt modMeta
        pUI = squareToUI $ PointSquare col row
    saveKMP rf modifier (decideMouseButton button) pUI

foreign export javascript "lhMouseUp"
  lhMouseUp :: Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> IO ()
#endif
