-- | Saving/loading to files, with serialization and compression.
module Game.LambdaHack.Common.HSFile
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile, renameFile
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , encodeData
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Codec.Compression.Zlib as Z
import qualified Control.Exception as Ex
import           Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as T
import           Data.Version
import           System.Directory
import           System.FilePath
import           System.IO
  ( IOMode (..)
  , hClose
  , hSetEncoding
  , localeEncoding
  , openBinaryFile
  , readFile
  , utf8
  , withBinaryFile
  , withFile
  )

-- | Serialize and save data.
-- Note that LBS.writeFile opens the file in binary mode.
encodeData :: Binary a => FilePath -> a -> IO ()
encodeData path a = do
  let tmpPath = path <.> "tmp"
  Ex.bracketOnError
    (openBinaryFile tmpPath WriteMode)
    (\h -> hClose h >> removeFile tmpPath)
    (\h -> do
       LBS.hPut h . encode $ a
       hClose h
       renameFile tmpPath path
    )

-- | Serialize, compress and save data with an EOF marker.
-- The @OK@ is used as an EOF marker to ensure any apparent problems with
-- corrupted files are reported to the user ASAP.
encodeEOF :: Binary b => FilePath -> Version -> b -> IO ()
encodeEOF path v b =
  encodeData path (v, (Z.compress $ encode b, "OK" :: String))

-- | Read, decompress and deserialize data with an EOF marker.
-- The @OK@ EOF marker ensures any easily detectable file corruption
-- is discovered and reported before any value is decoded from
-- the second component and before the file handle is closed.
-- OTOH, binary encoding corruption is not discovered until a version
-- check elswere ensures that binary formats are compatible.
strictDecodeEOF :: Binary b => FilePath -> IO (Version, b)
strictDecodeEOF path =
  withBinaryFile path ReadMode $ \h -> do
    c1 <- LBS.hGetContents h
    let (v1, (c2, s)) = decode c1
    return $! if s == ("OK" :: String)
              then (v1, decode $ Z.decompress c2)
              else error $ "Fatal error: corrupted file " ++ path

-- | Try to create a directory, if it doesn't exist. We catch exceptions
-- in case many clients try to do the same thing at the same time.
tryCreateDir :: FilePath -> IO ()
tryCreateDir dir = do
  dirExists <- doesDirectoryExist dir
  unless dirExists $
    Ex.handle (\(_ :: Ex.IOException) -> return ())
              (createDirectory dir)

-- | Try to write a file, given content, if the file not already there.
-- We catch exceptions in case many clients and/or the server try to do
-- the same thing at the same time. Using `Text.IO` to avoid UTF conflicts
-- with OS or filesystem.
tryWriteFile :: FilePath -> Text -> IO ()
tryWriteFile path content = do
  fileExists <- doesFileExist path
  unless fileExists $ do
    -- With some luck, locale was already corrected in Main.hs, but just
    -- in case, we make sure not to save UTF files in too primitve encodings.
    let enc = localeEncoding
    Ex.handle (\(ex :: Ex.IOException) -> print $ show ex) $
      withFile path WriteMode $ \h -> do
        when (show enc `elem` ["ASCII", "ISO-8859-1", "ISO-8859-2"]) $
          hSetEncoding h utf8
        T.hPutStr h content
