{-# LANGUAGE CPP #-}
-- | Saving/loading with serialization and compression.
module Game.LambdaHack.Server.FileServer
  ( encodeEOF, strictDecodeEOF, tryCreateDir, tryCopyDataFiles, appDataDir
  ) where

#ifdef USE_JSFILE_SERVER
import Game.LambdaHack.Common.JSFile
#else
import Game.LambdaHack.Common.HSFile
#endif
