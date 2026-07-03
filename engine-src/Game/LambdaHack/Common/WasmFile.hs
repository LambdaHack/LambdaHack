{-# LANGUAGE ForeignFunctionInterface #-}
-- | Saving/loading to JS storage, mimicking operations on files, for the
-- wasm32-wasi frontend. Mirrors JSFile.hs (the GHCJS equivalent) exactly in
-- storage format and behavior -- same lz-string compression, same
-- EOF-marker scheme, same raw @path@-as-key convention -- but via GHC's wasm
-- JSFFI (GHC.Wasm.Prim) instead of ghcjs-dom, since wasm32-wasi has no
-- DOM/Window binding layer and localStorage is reached as a plain JS global.
--
-- This deliberately bypasses the WASI virtual filesystem (the empty
-- in-memory PreopenDirectory the loader sets up for stdio) entirely: saves
-- go straight to localStorage via JSFFI, the same way Wasm.hs's lhKey/
-- lhWheel/lhMouseUp bypass a "real" input layer. Faking POSIX
-- directory/rename semantics over a key-value store inside the WASI shim
-- would be substantially more complex than this, for no benefit here.
--
-- Requires @lz-string@ to be loaded as a page-global (@globalThis.LZString@)
-- alongside the wasm bundle -- not currently the case in web/index.html,
-- which needs a script tag added (see lz-string.min.js at the repo root,
-- already used by the GHCJS build for the same purpose).
module Game.LambdaHack.Common.WasmFile
  (
#ifdef USE_WASMFILE
-- to molify doctest, but don't break stylish-haskell parsing
    encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile, renameFile
#endif
  ) where

#ifdef USE_WASMFILE
import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import           Data.Text.Encoding (decodeLatin1)
import           Data.Version
import           GHC.Wasm.Prim (JSString (..), fromJSString, toJSString)

-- | Same lz-string routines JSFile.hs uses, reached as a page-global instead
-- of via a GHCJS-specific import, since wasm32-wasi has no such binding.
foreign import javascript unsafe "globalThis.LZString.compressToUTF16($1)"
  compressToUTF16 :: JSString -> IO JSString

foreign import javascript unsafe "globalThis.LZString.decompressFromUTF16($1)"
  decompressFromUTF16 :: JSString -> IO JSString

-- | Split into a presence check plus an unconditional get (rather than one
-- call returning a nullable/Maybe result, as ghcjs-dom's Storage.getItem
-- does) because GHC's wasm JSFFI has no built-in Maybe-JSString marshaling.
-- Callers must check existence before calling js_getItem.
foreign import javascript unsafe "globalThis.localStorage.getItem($1) !== null"
  js_hasItem :: JSString -> IO Bool

foreign import javascript unsafe "globalThis.localStorage.getItem($1)"
  js_getItem :: JSString -> IO JSString

foreign import javascript unsafe "globalThis.localStorage.setItem($1, $2)"
  js_setItem :: JSString -> JSString -> IO ()

foreign import javascript unsafe "globalThis.localStorage.removeItem($1)"
  js_removeItem :: JSString -> IO ()

-- | Serialize and save data with an EOF marker, compressing. Identical
-- format to JSFile.hs's encodeEOF: treat the bytestring as Latin1 characters
-- (so every byte round-trips as a single UTF-16 code unit, 0-255, avoiding
-- illegal-character issues arbitrary binary data would hit as real UTF-16)
-- before compressing. The @OK@ marker flags corruption ASAP on read.
encodeEOF :: Binary b => FilePath -> Version -> b -> IO ()
encodeEOF path v b = do
  let t = decodeLatin1 $ LBS.toStrict $ encode (v, (encode b, "OK" :: String))
  item <- compressToUTF16 $ toJSString $ T.unpack t
  js_setItem (toJSString path) item

-- | Read and deserialize data with an EOF marker. Identical format/checks to
-- JSFile.hs's strictDecodeEOF.
strictDecodeEOF :: Binary b => FilePath -> IO (Version, b)
strictDecodeEOF path = do
  exists <- js_hasItem (toJSString path)
  unless exists $ fail $ "Fatal error: no file " ++ path
  item <- js_getItem (toJSString path)
  t <- decompressFromUTF16 item
  let c1 = LBS.pack $ fromJSString t
      (v1, (c2, s)) = decode c1
  return $! if s == ("OK" :: String)
            then (v1, decode c2)
            else error $ "Fatal error: corrupted file " ++ path

-- | Try to create a directory; not needed with local storage in JS.
tryCreateDir :: FilePath -> IO ()
tryCreateDir _dir = return ()

doesFileExist :: FilePath -> IO Bool
doesFileExist path = js_hasItem (toJSString path)

-- | Takes Text, not String, matching HSFile.hs's real signature -- JSFile.hs
-- states String here but its body calls T.unpack on the argument, which
-- only type-checks if it's actually Text; that looks like a pre-existing
-- inaccuracy over there, not something to mirror.
tryWriteFile :: FilePath -> Text -> IO ()
tryWriteFile path content = do
  exists <- js_hasItem (toJSString path)
  unless exists $ js_setItem (toJSString path) (toJSString $ T.unpack content)

readFile :: FilePath -> IO String
readFile path = do
  exists <- js_hasItem (toJSString path)
  unless exists $ fail $ "Fatal error: no file " ++ path
  item <- js_getItem (toJSString path)
  return $! fromJSString item

renameFile :: FilePath -> FilePath -> IO ()
renameFile path path2 = do
  exists <- js_hasItem (toJSString path)
  unless exists $ fail $ "Fatal error: no file " ++ path
  item <- js_getItem (toJSString path)
  js_setItem (toJSString path2) item  -- overwrites
  js_removeItem (toJSString path)
#endif
