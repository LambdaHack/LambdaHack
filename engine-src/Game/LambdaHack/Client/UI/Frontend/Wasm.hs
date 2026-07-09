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
import qualified Data.Vector.Storable as VS
import           Foreign.Ptr (minusPtr, nullPtr)
import           GHC.Wasm.Prim (JSString(..), fromJSString)
import           System.IO.Unsafe (unsafePerformIO)

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions

-- | Paint a frame: @(address, width, height)@ of a @Word32@ cell buffer in
-- wasm linear memory. The JS side (set up by the loader
-- as @globalThis.lhPaint@) reads the buffer and renders.
-- @unsafe@: synchronous, so no GC moves the buffer mid-call.
foreign import javascript unsafe "globalThis.lhPaint($1, $2, $3)"
  js_paint :: Int -> Int -> Int -> IO ()

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
  VS.unsafeWith (faVector singleArray) $ \ptr ->
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
        modifierNoShift = case modifier of  -- to prevent S-!, etc.
          K.Shift -> K.NoModifier
          K.ControlShift -> K.Control
          K.AltShift -> K.Alt
          _ -> modifier
    when (key == K.Esc) $ resetChanKey (fchanKey rf)
    saveKMP rf modifierNoShift key (PointUI 0 0)

foreign export javascript "lhKey"
  lhKey :: JSString -> Bool -> Bool -> Bool -> Bool -> IO ()

-- | Handle a wheel event delivered from JS over a given screen cell
-- (0-based column/row). Only deltaY beyond a small epsilon counts,
-- to filter out zero-delta glitches.
lhWheel :: Int -> Int -> Double -> Bool -> Bool -> Bool -> Bool -> IO ()
lhWheel col row deltaY modCtrl modShift modAlt modMeta = do
  mrf <- readIORef rfRef
  forM_ mrf $ \rf -> do
    let modifier = modifierTranslate modCtrl modShift modAlt modMeta
        pUI = squareToUI $ PointSquare col row
        mkey | deltaY < -0.01 = Just K.WheelNorth
             | deltaY > 0.01 = Just K.WheelSouth
             | otherwise = Nothing  -- probably a glitch
    forM_ mkey $ \key -> saveKMP rf modifier key pUI

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
        key = case button of
          0 -> K.LeftButtonRelease
          1 -> K.MiddleButtonRelease
          2 -> K.RightButtonRelease  -- not handled in contextmenu
          _ -> K.LeftButtonRelease  -- any other is alternate left
    saveKMP rf modifier key pUI

foreign export javascript "lhMouseUp"
  lhMouseUp :: Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> IO ()
#endif
