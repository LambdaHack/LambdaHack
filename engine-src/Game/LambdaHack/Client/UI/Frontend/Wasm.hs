{-# LANGUAGE MagicHash #-}
-- | Text frontend running in a browser via the wasm32-wasi backend, driven by
-- a thin TypeScript terminal-emulator over the GHC wasm JSFFI.
--
-- Display: each frame's raw @Word32@ cell array (the AttrCharW32 encoding:
-- @char = w >> 16@, @fg = (w >> 8) & 0xFF@, @bg = w & 0xFF@) is handed to JS by
-- address into wasm linear memory; the TS side decodes and paints. Input: JS
-- calls the exported @lhKey@ with each keydown event in the terminal, and
-- @lhKey@ decides the browser's default action and translates and queues
-- the key. Screenshots: a name is handed to JS, which saves the screen
-- under it. Startup: the font scale the page draws by and the fonts it draws
-- the overlays in are handed to JS once, as JSON.
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
import qualified Data.Char as Char
import           Data.IORef
import           Data.Primitive.ByteArray (ByteArray (..), ByteArray#)
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.LocalTime (getTimeZone)
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as VU
import           Data.Word (Word32)
import           Foreign.Storable (sizeOf)
import           GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)
import           System.FilePath ((<.>))
import           System.IO (hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf (printf)

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import           Game.LambdaHack.Client.UI.Frontend.InputDecision
import           Game.LambdaHack.Client.UI.Frontend.Screenshot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Misc (FontDefinition (..), FontSet (..))
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
startup coscreen soptions@ClientOptions{sprintEachScreen} = do
  js_configure $ toJSString $ startupPayload soptions
  rfWithoutPrintScreen <-
    createRawFrontend coscreen (display coscreen sprintEachScreen) shutdown
  let rf = rfWithoutPrintScreen {fprintScreen = printScreen}
  writeIORef rfRef $ Just rf
  return rf

-- | The startup payload: a JSON object of named fields, each present when
-- the options carry its value. @allFontsScale@ is the scale @Sdl.hs@
-- multiplies its font sizes by. @fonts@ names the chosen fontset's fonts
-- but the map font, under @propRegular@, @propBold@ and @mono@, each a file
-- name and a size in pixels, multiplied by that scale as @Sdl.hs@ multiplies
-- it; a bitmap font, which has neither a size to scale nor a form a browser
-- loads, is left out.
startupPayload :: ClientOptions -> String
startupPayload
  ClientOptions{sallFontsScale, schosenFontset, sfontsets, sfonts} =
  jsonObject $ [ ("allFontsScale", show scale)
               | Just scale <- [sallFontsScale] ]
               ++ [ ("fonts", jsonObject fonts) | not (null fonts) ]
 where
  fonts =
    [ ( field
      , jsonObject [ ("file", jsonString $ T.unpack fname)
                   , ("size", show size) ] )
    | Just scale <- [sallFontsScale]
    , Just fontsetID <- [schosenFontset]
    , Just fontset <- [lookup fontsetID sfontsets]
    , (field, fontName) <- [ ("propRegular", fontPropRegular fontset)
                           , ("propBold", fontPropBold fontset)
                           , ("mono", fontMono fontset) ]
    , Just (fname, fsize) <- [lookup fontName sfonts >>= outlineFont]
    , let size = round $ scale * intToDouble fsize :: Int ]
  outlineFont = \case
    FontProportional fname fsize _ -> Just (fname, fsize)
    FontMonospace fname fsize _ -> Just (fname, fsize)
    FontMapScalable fname fsize _ _ -> Just (fname, fsize)
    FontMapBitmap{} -> Nothing

-- | A JSON object of the given fields, whose values are JSON already.
jsonObject :: [(String, String)] -> String
jsonObject fields =
  "{" ++ intercalate "," [jsonString k ++ ":" ++ v | (k, v) <- fields] ++ "}"

-- | A JSON string literal.
jsonString :: String -> String
jsonString s = "\"" ++ concatMap escape s ++ "\""
 where
  escape c | c == '"' || c == '\\' = ['\\', c]
           | c < ' ' = printf "\\u%04x" (Char.ord c)
           | otherwise = [c]

-- | Hand the page the startup payload, before the first frame. The JS side
-- (set up by the loader as @globalThis.lhConfigure@) parses it, sizes
-- the screen by it and loads the fonts it names. One call carries every
-- field, so that a new one changes no signature, and JSON, which a frame's
-- cost rules out, is cheap once per game.
foreign import javascript unsafe "globalThis.lhConfigure($1)"
  js_configure :: JSString -> IO ()

shutdown :: IO ()
shutdown = return () -- nothing to clean up

-- | Output to the screen via the frontend, and print each screen
-- if so asked.
display :: ScreenContent -> Bool -> SingleFrame -> IO ()
display ScreenContent{rwidth, rheight} printEachScreen
        SingleFrame{singleArray} = do
  let v = PointArray.avector singleArray :: U.Vector Word32
  word32BackingArray v $ \ba# byteOffset ->
    js_submitFrame ba# byteOffset rwidth rheight
  when printEachScreen printScreen

-- | Save a screenshot, named as SDL names its own, with the extension
-- of the PNG the canvas exports in place of SDL's bitmap.
printScreen :: IO ()
printScreen = do
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  js_printScreen $ toJSString $ screenshotName timezone utcTime <.> "png"

-- | Save a screenshot under the given file name. The JS side (set up
-- by the loader as @globalThis.lhPrintScreen@) saves the screen as it is
-- once the frame submitted last is painted.
foreign import javascript unsafe "globalThis.lhPrintScreen($1)"
  js_printScreen :: JSString -> IO ()

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

-- | Handle a wheel event delivered from JS over a given screen point:
-- the 0-based 'PointUI' column, in half-cells, as @Sdl.hs@'s
-- @pointTranslate@ gives it, and the 0-based row.
lhWheel :: Int -> Int -> Double -> Bool -> Bool -> Bool -> Bool -> IO ()
lhWheel x row deltaY modCtrl modShift modAlt modMeta = do
  mrf <- readIORef rfRef
  forM_ mrf $ \rf -> do
    let modifier = modifierTranslate modCtrl modShift modAlt modMeta
    forM_ (decideWheel deltaY) $ \key ->
      saveKMP rf modifier key (PointUI x row)

foreign export javascript "lhWheel"
  lhWheel :: Int -> Int -> Double -> Bool -> Bool -> Bool -> Bool -> IO ()

-- | Handle a mouseup delivered from JS over a given screen point, given
-- as to 'lhWheel', with the DOM @MouseEvent.button@ code.
lhMouseUp :: Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> IO ()
lhMouseUp x row button modCtrl modShift modAlt modMeta = do
  mrf <- readIORef rfRef
  forM_ mrf $ \rf -> do
    let modifier = modifierTranslate modCtrl modShift modAlt modMeta
    saveKMP rf modifier (decideMouseButton button) (PointUI x row)

foreign export javascript "lhMouseUp"
  lhMouseUp :: Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> IO ()
#endif
