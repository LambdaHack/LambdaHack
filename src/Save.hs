module Save where

import System.Directory
import Control.Exception as E hiding (handle)

import File
import Level
import State
import qualified Config

-- | Name of the save game.
file :: Config.CP -> IO String
file config = Config.getFile config "LambdaHack.save" "files" "savegame"

-- | We save a simple serialized version of the current level and
-- the current state. The 'False' is used only as an EOF marker.
saveGame :: State -> IO ()
saveGame state =
  do
    f <- file (config state)
    encodeCompressedFile f (state,False)

-- | Restore a saved game. Returns either the current level and
-- game state, or a string containing an error message if restoring
-- the game fails.
restoreGame :: Config.CP -> IO (Either State String)
restoreGame config =
  E.catch (do
             f <- file config
             r <- strictDecodeCompressedFile f
             removeFile f
             case r of
               (x,z) -> (z :: Bool) `seq` return $ Left x)
          (\ e -> case e :: IOException of
                    _ -> return (Right $ "Restore failed: " ++
                                 (unwords . lines) (show e)))
