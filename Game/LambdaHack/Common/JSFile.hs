-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Common.JSFile
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile
  , domContextUnsafe
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char as Char
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import GHCJS.DOM (currentWindow, postGUISync, runWebGUI)
import GHCJS.DOM.Storage (getItem, setItem)
import GHCJS.DOM.Types (DOMContext, runDOM)
import GHCJS.DOM.Types (askDOM)
import GHCJS.DOM.Window (getLocalStorage)
import System.Directory (getAppUserDataDirectory)
import System.Environment (getProgName)
import System.IO.Unsafe (unsafePerformIO)

domContextUnsafe :: DOMContext
{-# NOINLINE domContextUnsafe #-}
domContextUnsafe = unsafePerformIO $ do
  rfMVar <- newEmptyMVar
  runWebGUI $ \_ -> do
    sdomContext <- askDOM
    IO.liftIO $ putMVar rfMVar sdomContext
  takeMVar rfMVar

-- | Serialize and save data with an EOF marker. In JS, compression
-- is probably performed by the browser and we don't have access
-- to the zlib library anyway, so we don't compress here.
-- The @OK@ is used as an EOF marker to ensure any apparent problems with
-- corrupted files are reported to the user ASAP.
encodeEOF :: Binary a => FilePath -> a -> IO ()
encodeEOF path a =
  flip runDOM domContextUnsafe $ postGUISync $ do
    Just win <- currentWindow
    Just storage <- getLocalStorage win
    -- TODO: probably very slow as well, as wastes half the Local Storage space:
    setItem storage path $ decodeLatin1 $ LBS.toStrict
                         $ encode (a, "OK" :: String)

-- | Read and deserialize data with an EOF marker.
-- The @OK@ EOF marker ensures any easily detectable file corruption
-- is discovered and reported before the function returns.
strictDecodeEOF :: Binary a => FilePath -> IO a
strictDecodeEOF path =
  flip runDOM domContextUnsafe $ postGUISync $ do
    Just win <- currentWindow
    Just storage <- getLocalStorage win
    Just item <- getItem storage path
    -- TODO: terribly slow:
    let (a, n) = decode $ LBS.pack $ T.unpack item
    if n == ("OK" :: String)
      then return $! a
      else error $ "Fatal error: corrupted file " ++ path

-- | Try to create a directory; not needed with local storage in JS.
tryCreateDir :: FilePath -> IO ()
tryCreateDir _dir = return ()

doesFileExist :: FilePath -> IO Bool
doesFileExist path =
  flip runDOM domContextUnsafe $ postGUISync $ do
    Just win <- currentWindow
    Just storage <- getLocalStorage win
    mitem <- getItem storage path
    let fileExists = isJust (mitem :: Maybe String)
    return $! fileExists

-- | Try to write a file, given content, if the file not already there.
tryWriteFile :: FilePath -> String -> IO ()
tryWriteFile path content =
  flip runDOM domContextUnsafe $ postGUISync $ do
    Just win <- currentWindow
    Just storage <- getLocalStorage win
    mitem <- getItem storage path
    let fileExists = isJust (mitem :: Maybe String)
    unless fileExists $
      setItem storage path content

readFile :: FilePath -> IO String
readFile path =
  flip runDOM domContextUnsafe $ postGUISync $ do
    Just win <- currentWindow
    Just storage <- getLocalStorage win
    mitem <- getItem storage path
    case mitem of
      Nothing -> error $ "Fatal error: no file " ++ path
      Just item -> return item
