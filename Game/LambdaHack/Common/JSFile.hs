{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Common.JSFile
  ( encodeEOF, strictDecodeEOF, tryCreateDir, tryWriteFile, appDataDir
  , domContextUnsafe
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import Data.Binary
import qualified Data.Char as Char
import GHCJS.DOM (currentWindow, postGUISync, runWebGUI)
import GHCJS.DOM.Storage (getItem, setItem)
import GHCJS.DOM.Types (DOMContext, runDOM)
import GHCJS.DOM.Types (askDOM)
import GHCJS.DOM.Window (getLocalStorage)
import System.Directory
import System.Environment
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
encodeEOF _f _a = return ()

-- | Read and deserialize data with an EOF marker.
-- The @OK@ EOF marker ensures any easily detectable file corruption
-- is discovered and reported before the function returns.
strictDecodeEOF :: Binary a => FilePath -> IO a
strictDecodeEOF _f = error "file handling not yet implemented on browser"

-- | Try to create a directory; not needed with local storage in JS.
tryCreateDir :: FilePath -> IO ()
tryCreateDir _dir = return ()

-- | Try to write a file, given content, if the file not already there.
tryWriteFile :: FilePath -> String -> IO ()
tryWriteFile path content = do
  flip runDOM domContextUnsafe $ postGUISync $ do
    Just win <- currentWindow
    Just storage <- getLocalStorage win
    item <- getItem storage path
    let fileExists = isJust (item :: Maybe String)
    unless fileExists $
      setItem storage path content

-- | Personal data directory for the game. Depends on the OS, the game, etc.;
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@, but in the browser
-- it's yet different.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name
