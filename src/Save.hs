module Save where

import System.Directory
import Control.Exception as E hiding (handle)

import Data.Maybe

import File
import Level
import State
import qualified Config

-- | Name of the save game. TODO: It should be possible to play
-- multiple games next to each other (for instance, in a multi-user
-- environment), so the savegames should contain the character name
-- and the user id or something like that.
file :: IO String
file =
  do
    s <- Config.getString "files" "savegame"
    return $ fromMaybe "LambdaHack.save" s


-- | We save a simple serialized version of the current level and
-- the current state. The 'False' is used only as an EOF marker.
saveGame :: State -> IO ()
saveGame state =
  do
    f <- file
    encodeCompressedFile f (state,False)

-- | Restore a saved game. Returns either the current level and
-- game state, or a string containing an error message if restoring
-- the game fails.
restoreGame :: IO (Either State String)
restoreGame =
  E.catch (do
             f <- file
             r <- strictDecodeCompressedFile f
             removeFile f
             case r of
               (x,z) -> (z :: Bool) `seq` return $ Left x)
          (\ e -> case e :: IOException of
                    _ -> return (Right $ "Restore failed: " ++
                                 (unwords . lines) (show e)))
