{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Common.JSFile
  ( encodeEOF, strictDecodeEOF, tryCreateDir, tryCopyDataFiles, appDataDir
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.Char as Char
import System.Directory
import System.Environment

-- TODO: implement using on of the data stores available in browsers
-- and one of the compression libs/packages/js sources

-- | Serialize, compress and save data with an EOF marker.
-- The @OK@ is used as an EOF marker to ensure any apparent problems with
-- corrupted files are reported to the user ASAP.
encodeEOF :: Binary a => FilePath -> a -> IO ()
encodeEOF _f _a = return ()

-- | Read, decompress and deserialize data with an EOF marker.
-- The @OK@ EOF marker ensures any easily detectable file corruption
-- is discovered and reported before the function returns.
strictDecodeEOF :: Binary a => FilePath -> IO a
strictDecodeEOF _f = error "file handling not yet implemented on browser"

-- | Try to create a directory, if it doesn't exist. We catch exceptions
-- in case many clients try to do the same thing at the same time.
tryCreateDir :: FilePath -> IO ()
tryCreateDir _dir = return ()

-- | Try to copy over data files, if not already there. We catch exceptions
-- in case many clients try to do the same thing at the same time.
tryCopyDataFiles :: FilePath
                 -> (FilePath -> IO FilePath)
                 -> [(FilePath, FilePath)]
                 -> IO ()
tryCopyDataFiles _dataDir _pathsDataFile _files = return ()

-- | Personal data directory for the game. Depends on the OS and the game,
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name
