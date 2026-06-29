{-# LANGUAGE ForeignFunctionInterface #-}
-- | Text frontend running in a browser via the wasm32-wasi backend, driven by
-- a thin TypeScript terminal-emulator over the GHC wasm JSFFI.
--
-- Display: each frame's raw @Word32@ cell array (the AttrCharW32 encoding:
-- @char = w >> 16@, @fg = (w >> 8) & 0xFF@, @bg = w & 0xFF@) is handed to JS by
-- address into wasm linear memory; the TS side decodes and paints. Input: JS
-- calls the exported @lhKey@ on keydown, which translates and queues the key.
module Game.LambdaHack.Client.UI.Frontend.Wasm
  (
#ifdef USE_WASM
   startup, frontendName
#endif
  ) where

#ifdef USE_WASM
import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.IORef
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word32)
import           Foreign.Ptr (minusPtr, nullPtr)
import           GHC.Wasm.Prim (JSString(..), fromJSString)
import           System.IO.Unsafe (unsafePerformIO)

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Paint a frame: @(address, width, height)@ of a @Word32@ cell buffer in
-- wasm linear memory. The JS side (set up by the loader as @globalThis.lhPaint@)
-- reads the buffer and renders. @unsafe@: synchronous, so no GC moves the buffer
-- mid-call.
foreign import javascript unsafe "globalThis.lhPaint($1, $2, $3)"
  js_paint :: Int -> Int -> Int -> IO ()

-- | The active frontend, stashed so the exported 'lhKey' can reach its key
-- queue. There is only ever one frontend per wasm instance.
{-# NOINLINE rfRef #-}
rfRef :: IORef (Maybe RawFrontend)
rfRef = unsafePerformIO $ newIORef Nothing

-- | The name of the frontend.
frontendName :: String
frontendName = "wasm"

-- | Set up the frontend input and output.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen _soptions = do
  rf <- createRawFrontend coscreen (display coscreen) (return ())
  writeIORef rfRef $ Just rf
  return rf

-- | Output to the screen via the frontend.
display :: ScreenContent -> SingleFrame -> IO ()
display ScreenContent{rwidth, rheight} SingleFrame{singleArray} =
  let v = PointArray.avector singleArray :: U.Vector Word32
  in VS.unsafeWith (VG.convert v) $ \ptr ->
       js_paint (ptr `minusPtr` nullPtr) rwidth rheight

-- | Handle a keydown delivered from JS: the @KeyboardEvent.key@ string plus the
-- ctrl/shift/alt/meta modifier flags. Mirrors the web (Dom) frontend's keydown
-- logic, including the shift squashing that prevents e.g. S-! .
lhKey :: JSString -> Bool -> Bool -> Bool -> Bool -> IO ()
lhKey jsKey modCtrl modShift modAlt modMeta = do
  mrf <- readIORef rfRef
  forM_ mrf $ \rf -> do
    let modifier = modifierTranslate modCtrl modShift modAlt modMeta
        key = K.keyTranslateWeb (fromJSString jsKey) (modifier == K.Shift)
        modifierNoShift = case modifier of
          K.Shift -> K.NoModifier
          K.ControlShift -> K.Control
          K.AltShift -> K.Alt
          _ -> modifier
    when (key == K.Esc) $ resetChanKey (fchanKey rf)
    saveKMP rf modifierNoShift key (PointUI 0 0)

foreign export javascript "lhKey"
  lhKey :: JSString -> Bool -> Bool -> Bool -> Bool -> IO ()
#endif
