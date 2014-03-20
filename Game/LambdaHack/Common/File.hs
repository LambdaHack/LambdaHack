-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Common.File
  ( encodeEOF, strictDecodeEOF, tryCreateDir, tryCopyDataFiles, appDataDir
  ) where

import qualified Codec.Compression.Zlib as Z
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import System.Directory
import System.Environment
import System.FilePath
import System.IO

-- | Serialize, compress and save data.
-- Note that LBS.writeFile opens the file in binary mode.
encodeData :: Binary a => FilePath -> a -> IO ()
encodeData f a = do
  let tmpPath = f <.> "tmp"
  Ex.bracketOnError
    (openBinaryFile tmpPath WriteMode)
    (\h -> hClose h >> removeFile tmpPath)
    (\h -> do
       LBS.hPut h . Z.compress . encode $ a
       hClose h
       renameFile tmpPath f
    )

-- | Serialize, compress and save data with an EOF marker.
-- The @OK@ is used as an EOF marker to ensure any apparent problems with
-- corrupted files are reported to the user ASAP.
encodeEOF :: Binary a => FilePath -> a -> IO ()
encodeEOF f a = encodeData f (a, "OK" :: String)

-- | Read and decompress the serialized data.
strictReadSerialized :: FilePath -> IO LBS.ByteString
strictReadSerialized f =
  withBinaryFile f ReadMode $ \ h -> do
    c <- LBS.hGetContents h
    let d = Z.decompress c
    LBS.length d `seq` return d

-- | Read, decompress and deserialize data.
strictDecodeData :: Binary a => FilePath -> IO a
strictDecodeData = fmap decode . strictReadSerialized

-- | Read, decompress and deserialize data with an EOF marker.
-- The @OK@ EOF marker ensures any easily detectable file corruption
-- is discovered and reported before the function returns.
strictDecodeEOF :: Binary a => FilePath -> IO a
strictDecodeEOF f = do
  (a, n) <- strictDecodeData f
  if n == ("OK" :: String)
    then return $! a
    else error $ "Fatal error: corrupted file " ++ f

-- | Try to create a directory, if it doesn't exist. Terminate the program
-- with an exception if the directory does not exist, but can't be created.
tryCreateDir :: FilePath -> IO ()
tryCreateDir dir = do
  dirExists <- doesDirectoryExist dir
  unless dirExists $ createDirectory dir

-- | Try to copy over data files, if not already there.
tryCopyDataFiles :: FilePath
                 -> (FilePath -> IO FilePath)
                 -> [(FilePath, FilePath)]
                 -> IO ()
tryCopyDataFiles dataDir pathsDataFile files =
  let cpFile (fin, fout) = do
        pathsDataIn <- pathsDataFile fin
        bIn <- doesFileExist pathsDataIn
        let pathsDataOut = dataDir </> fout
        bOut <- doesFileExist pathsDataOut
        when (not bOut && bIn) $ copyFile pathsDataIn pathsDataOut
  in mapM_ cpFile files

-- | Personal data directory for the game. Depends on the OS and the game,
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name
