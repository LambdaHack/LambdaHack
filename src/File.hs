module File where

import System.IO
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as Z

strictReadCompressedFile :: FilePath -> IO LBS.ByteString
strictReadCompressedFile f =
    do
      h <- openBinaryFile f ReadMode
      c <- LBS.hGetContents h
      let d = Z.decompress c
      LBS.length d `seq` return d

strictDecodeCompressedFile :: Binary a => FilePath -> IO a
strictDecodeCompressedFile = fmap decode . strictReadCompressedFile

encodeCompressedFile :: Binary a => FilePath -> a -> IO ()
encodeCompressedFile f = LBS.writeFile f . Z.compress . encode
  -- note that LBS.writeFile opens the file in binary mode
