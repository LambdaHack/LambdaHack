{-# LANGUAGE CPP #-}
-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Client.FileClient
  ( encodeEOF, strictDecodeEOF, tryCreateDir, tryCopyDataFiles, appDataDir
  ) where

#ifdef USE_JSFILE_CLIENT
import Game.LambdaHack.Common.JSFile
#else
import Game.LambdaHack.Common.HSFile
#endif
