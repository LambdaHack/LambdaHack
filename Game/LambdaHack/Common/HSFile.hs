-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Common.HSFile
  ( encodeEOF, strictDecodeEOF
  , tryCreateDir, doesFileExist, tryWriteFile, readFile, renameFile
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Codec.Compression.Zlib as Z
import qualified Control.Exception as Ex
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import System.Directory
import System.FilePath
import System.IO (IOMode (..), hClose, openBinaryFile, readFile, withBinaryFile,
                  writeFile)

-- | Serialize, compress and save data.
-- Note that LBS.writeFile opens the file in binary mode.
encodeData :: Binary a => FilePath -> a -> IO ()
encodeData path a = do
  let tmpPath = path <.> "tmp"
  Ex.bracketOnError
    (openBinaryFile tmpPath WriteMode)
    (\h -> hClose h >> removeFile tmpPath)
    (\h -> do
       LBS.hPut h . Z.compress . encode $ a
       hClose h
       renameFile tmpPath path
    )

-- | Serialize, compress and save data with an EOF marker.
-- The @OK@ is used as an EOF marker to ensure any apparent problems with
-- corrupted files are reported to the user ASAP.
encodeEOF :: Binary a => FilePath -> a -> IO ()
encodeEOF path a = encodeData path (a, "OK" :: String)

-- | Read, decompress and deserialize data with an EOF marker.
-- The @OK@ EOF marker ensures any easily detectable file corruption
-- is discovered and reported before the function returns.
strictDecodeEOF :: Binary a => FilePath -> IO a
strictDecodeEOF path =
  withBinaryFile path ReadMode $ \h -> do
    c <- LBS.hGetContents h
    let (a, n) = decode $ Z.decompress c
    if n == ("OK" :: String)
      then return $! a
      else assert `failure` "Fatal error: corrupted file " ++ path

-- | Try to create a directory, if it doesn't exist. We catch exceptions
-- in case many clients try to do the same thing at the same time.
tryCreateDir :: FilePath -> IO ()
tryCreateDir dir = do
  dirExists <- doesDirectoryExist dir
  unless dirExists $
    Ex.handle (\(_ :: Ex.IOException) -> return ())
              (createDirectory dir)

-- | Try to write a file, given content, if the file not already there.
-- We catch exceptions in case many clients try to do the same thing
-- at the same time.
tryWriteFile :: FilePath -> String -> IO ()
tryWriteFile path content = do
  fileExists <- doesFileExist path
  unless fileExists $
    Ex.handle (\(_ :: Ex.IOException) -> return ())
              (writeFile path content)
