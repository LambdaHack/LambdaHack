-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Utils.File
  ( encodeEOF, strictDecodeEOF
  ) where

import System.IO
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as Z

-- | Serialize, compress and save data.
-- Note that LBS.writeFile opens the file in binary mode.
encodeData :: Binary a => FilePath -> a -> IO ()
encodeData f = LBS.writeFile f . Z.compress . encode

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
    then return a
    else error $ "Fatal error: corrupted file " ++ f
