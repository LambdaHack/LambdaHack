{-# LANGUAGE MagicHash, UnliftedFFITypes #-}
-- | Saving/loading to JS storage, mimicking operations on files, for the
-- wasm32-wasi frontend. Mirrors JSFile.hs (the GHCJS equivalent) exactly in
-- storage format -- same lz-string compression, same EOF-marker scheme, same
-- raw @path@-as-key convention -- but via GHC's wasm JSFFI (GHC.Wasm.Prim)
-- instead of ghcjs-dom, since wasm32-wasi has no DOM/Window binding layer
-- and localStorage is reached as a plain JS global.
--
-- This deliberately bypasses the WASI virtual filesystem (the empty
-- in-memory PreopenDirectory the loader sets up for stdio) entirely: saves
-- go straight to localStorage via JSFFI, the same way Wasm.hs's lhKey/
-- lhWheel/lhMouseUp bypass a "real" input layer. Faking POSIX
-- directory/rename semantics over a key-value store inside the WASI shim
-- would be substantially more complex than this, for no benefit here.
--
-- Requires @lz-string@ to be loaded as a page-global (@globalThis.LZString@)
-- alongside the wasm bundle.
module Game.LambdaHack.Common.WasmFile
  (
#ifdef USE_WASMFILE
-- to molify doctest, but don't break stylish-haskell parsing
    encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile, renameFile
    -- * Operations both internal and used in unit tests
  , compressToUTF16, decompressFromUTF16
#endif
  ) where

#ifdef USE_WASMFILE
import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (create)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Array as TA
import           Data.Text.Encoding (decodeLatin1)
import           Data.Text.Internal (Text (..))
import           Data.Version
import           Foreign.Ptr (Ptr)
import           GHC.Exts (ByteArray#)
import           GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)

-- | Same lz-string routines JSFile.hs uses, reached as a page-global instead
-- of via a GHCJS-specific import, since wasm32-wasi has no such binding.
foreign import javascript unsafe "globalThis.LZString.compressToUTF16($1)"
  compressToUTF16 :: JSString -> IO JSString

foreign import javascript unsafe "globalThis.LZString.decompressFromUTF16($1)"
  decompressFromUTF16 :: JSString -> IO JSString

-- | The stored item, null where there is none: a 'JSVal', since GHC's
-- wasm JSFFI marshals no nullable string, so that an item is fetched once
-- and copied nowhere to tell whether it is there.
foreign import javascript unsafe "globalThis.localStorage.getItem($1)"
  js_getItem :: JSString -> IO JSVal

foreign import javascript unsafe "$1 === null"
  js_isNull :: JSVal -> Bool

foreign import javascript unsafe "globalThis.localStorage.setItem($1, $2)"
  js_setItem :: JSString -> JSString -> IO ()

-- | Store an item, returning the error the storage throws where it refuses,
-- a full one among them, and null where it stores the item: a throw out
-- of an unsafe import is no Haskell exception, so it is caught here.
foreign import javascript unsafe
  "try { globalThis.localStorage.setItem($1, $2); return null; } catch (e) { return String(e); }"
  js_trySetItem :: JSString -> JSString -> IO JSVal

foreign import javascript unsafe "globalThis.localStorage.removeItem($1)"
  js_removeItem :: JSString -> IO ()

-- | The string a run of UTF-8 bytes in the payload of a 'ByteArray#'
-- encodes, at an offset and of a length in bytes. The payload is read
-- during the call, which runs no Haskell, so no GC moves it meanwhile.
foreign import javascript unsafe
  "new TextDecoder('utf-8', {ignoreBOM: true}).decode(new Uint8Array(__exports.memory.buffer, $1 + $2, $3))"
  js_decodeUTF8 :: ByteArray# -> Int -> Int -> IO JSString

foreign import javascript unsafe "$1.length"
  js_length :: JSString -> IO Int

-- | Write the code units of a string, each below 256, a byte apiece,
-- into a buffer as long as the string.
foreign import javascript unsafe
  "const b = new Uint8Array(__exports.memory.buffer, $2, $1.length); for (let i = 0; i < b.length; i++) b[i] = $1.charCodeAt(i);"
  js_writeLatin1 :: JSString -> Ptr Word8 -> IO ()

-- | The string of a 'Text', from its UTF-8 bytes, with no 'String' between.
textToJSString :: Text -> IO JSString
textToJSString (Text (TA.ByteArray arr) off len) = js_decodeUTF8 arr off len

-- | The bytes a string of code units below 256 holds, with no 'String'
-- between: the inverse of 'decodeLatin1' followed by 'textToJSString'.
latin1ToByteString :: JSString -> IO BS.ByteString
latin1ToByteString s = do
  n <- js_length s
  BS.create n $ js_writeLatin1 s

-- | Serialize and save data with an EOF marker, compressing. Identical
-- format to JSFile.hs's encodeEOF: treat the bytestring as Latin1 characters
-- (so every byte round-trips as a single UTF-16 code unit, 0-255, avoiding
-- illegal-character issues arbitrary binary data would hit as real UTF-16)
-- before compressing. The @OK@ marker flags corruption ASAP on read.
encodeEOF :: Binary b => FilePath -> Version -> b -> IO ()
encodeEOF path v b = do
  let t = decodeLatin1 $ LBS.toStrict $ encode (v, (encode b, "OK" :: String))
  item <- compressToUTF16 =<< textToJSString t
  js_setItem (toJSString path) item

-- | Read and deserialize data with an EOF marker. Identical format/checks to
-- JSFile.hs's strictDecodeEOF.
strictDecodeEOF :: Binary b => FilePath -> IO (Version, b)
strictDecodeEOF path = do
  item <- getExistingItem path
  c1 <- latin1ToByteString =<< decompressFromUTF16 item
  let (v1, (c2, s)) = decode $ LBS.fromStrict c1
  return $! if s == ("OK" :: String)
            then (v1, decode c2)
            else error $ "Fatal error: corrupted file " ++ path

-- | Try to create a directory; not needed with local storage in JS.
tryCreateDir :: FilePath -> IO ()
tryCreateDir _dir = return ()

doesFileExist :: FilePath -> IO Bool
doesFileExist path = isJust <$> getItem path

-- | Takes Text, not String, matching HSFile.hs's real signature, and
-- prints the error of a refused write, as HSFile.hs prints its exception.
tryWriteFile :: FilePath -> Text -> IO ()
tryWriteFile path content = do
  exists <- doesFileExist path
  unless exists $ do
    err <- js_trySetItem (toJSString path) =<< textToJSString content
    unless (js_isNull err) $ print $ fromJSString $ JSString err

readFile :: FilePath -> IO String
readFile path = do
  item <- getExistingItem path
  return $! fromJSString item

renameFile :: FilePath -> FilePath -> IO ()
renameFile path path2 = do
  item <- getExistingItem path
  js_setItem (toJSString path2) item  -- overwrites
  js_removeItem (toJSString path)

-- | The stored item, if any.
getItem :: FilePath -> IO (Maybe JSString)
getItem path = do
  v <- js_getItem $ toJSString path
  return $! if js_isNull v then Nothing else Just $ JSString v

-- | The stored item, failing if there is none.
getExistingItem :: FilePath -> IO JSString
getExistingItem path =
  getItem path >>= maybe (fail $ "Fatal error: no file " ++ path) return
#endif
