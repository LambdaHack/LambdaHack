module Game.LambdaHack.Utils.File
  ( encodeEOF, strictDecodeEOF
  ) where

import System.IO
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as Z

-- Note that LBS.writeFile opens the file in binary mode.
encodeCompressedFile :: Binary a => FilePath -> a -> IO ()
encodeCompressedFile f = LBS.writeFile f . Z.compress . encode

-- The "OK" is used as an EOF marker to ensure any problems with
-- corrupted files are reported to the user ASAP.
encodeEOF :: Binary a => FilePath -> a -> IO ()
encodeEOF f a = encodeCompressedFile f (a, "OK")

strictReadCompressedFile :: FilePath -> IO LBS.ByteString
strictReadCompressedFile f =
  withBinaryFile f ReadMode $ \ h -> do
    c <- LBS.hGetContents h
    let d = Z.decompress c
    LBS.length d `seq` return d

strictDecodeCompressedFile :: Binary a => FilePath -> IO a
strictDecodeCompressedFile = fmap decode . strictReadCompressedFile

-- The "OK" EOF marker ensures any detectable file corruption
-- is discovered and reported before the function returns.
strictDecodeEOF :: Binary a => FilePath -> IO a
strictDecodeEOF f = do
  (a, n) <- strictDecodeCompressedFile f
  if n == "OK"
    then return a
    else error $ "Fatal error: corrupted file " ++ f
