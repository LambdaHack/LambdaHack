{-# LANGUAGE JavaScriptFFI #-}
-- | Saving/loading to JS storeage, mimicking operations on files.
module Game.LambdaHack.Common.JSFile
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile, renameFile
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text.Encoding (decodeLatin1)
import           Data.Version

import qualified Data.JSString as JSString
import           Data.JSString.Text (textToJSString)
import           GHCJS.DOM (currentWindow)
import           GHCJS.DOM.Storage (getItem, removeItem, setItem)
import           GHCJS.DOM.Types (JSString, runDOM)
import           GHCJS.DOM.Window (getLocalStorage)

foreign import javascript safe "$r = LZString.compressToUTF16($1);"
  compressToUTF16 :: JSString -> IO JSString

foreign import javascript safe "$r = LZString.decompressFromUTF16($1);"
  decompressFromUTF16 :: JSString -> IO JSString

-- | Serialize and save data with an EOF marker, compressing.
-- We treat the bytestring as Latin1 characters and so ensure
-- we never run into illegal characters in the aribtrary binary data,
-- unlike when treating it as UTF16 characters. This is also reasonably fast.
-- The @OK@ is used as an EOF marker to ensure any apparent problems with
-- corrupted files are reported to the user ASAP.
encodeEOF :: Binary b => FilePath -> Version -> b -> IO ()
encodeEOF path v b = flip runDOM undefined $ do
  Just win <- currentWindow
  storage <- getLocalStorage win
  let t = decodeLatin1 $ LBS.toStrict $ encode (v, (encode b, "OK" :: String))
  item <- compressToUTF16 $ textToJSString t
  setItem storage path item

-- | Read and deserialize data with an EOF marker.
-- The @OK@ EOF marker ensures any easily detectable file corruption
-- is discovered and reported before any value is decoded from
-- the second component.
-- OTOH, binary encoding corruption is not discovered until a version
-- check elsewhere ensures that binary formats are compatible.
strictDecodeEOF :: Binary b => FilePath -> IO (Version, b)
strictDecodeEOF path = flip runDOM undefined $ do
  Just win <- currentWindow
  storage <- getLocalStorage win
  Just item <- getItem storage path
  t <- decompressFromUTF16 item
  -- TODO: is @LBS.toLazy . encodeUtf8 . textFromJSString@ faster and correct?
  let c1 = LBS.pack $ JSString.unpack t
      (v1, (c2, s)) = decode c1
  return $! if s == ("OK" :: String)
            then (v1, decode c2)
            else error $ "Fatal error: corrupted file " ++ path

-- | Try to create a directory; not needed with local storage in JS.
tryCreateDir :: FilePath -> IO ()
tryCreateDir _dir = return ()

doesFileExist :: FilePath -> IO Bool
doesFileExist path = flip runDOM undefined $ do
  Just win <- currentWindow
  storage <- getLocalStorage win
  mitem <- getItem storage path
  let fileExists = isJust (mitem :: Maybe String)
  return $! fileExists

tryWriteFile :: FilePath -> String -> IO ()
tryWriteFile path content = flip runDOM undefined $ do
  Just win <- currentWindow
  storage <- getLocalStorage win
  mitem <- getItem storage path
  let fileExists = isJust (mitem :: Maybe String)
  unless fileExists $
    setItem storage path content

readFile :: FilePath -> IO String
readFile path = flip runDOM undefined $ do
  Just win <- currentWindow
  storage <- getLocalStorage win
  mitem <- getItem storage path
  case mitem of
    Nothing -> fail $ "Fatal error: no file " ++ path
    Just item -> return item

renameFile :: FilePath -> FilePath -> IO ()
renameFile path path2 = flip runDOM undefined $ do
  Just win <- currentWindow
  storage <- getLocalStorage win
  mitem <- getItem storage path
  case mitem :: Maybe String of
    Nothing -> fail $ "Fatal error: no file " ++ path
    Just item -> do
      setItem storage path2 item  -- overwrites
      removeItem storage path
