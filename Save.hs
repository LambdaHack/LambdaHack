module Save where

import System.Directory
import Control.Exception as E hiding (handle)

import File
import Level
import State

-- | Name of the save game. TODO: It should be possible to play
-- multiple games next to each other (for instance, in a multi-user
-- environment), so the savegames should contain the character name
-- and the user id or something like that.
savefile = "LambdaHack.save"

-- | We save a simple serialized version of the current level and
-- the current state. The 'False' is used only as an EOF marker.
saveGame :: Level -> State -> IO ()
saveGame lvl state = encodeCompressedFile savefile (lvl,state,False)

-- | Restore a saved game. Returns either the current level and
-- game state, or a string containing an error message if restoring
-- the game fails.
restoreGame :: IO (Either (Level, State) String)
restoreGame =
  E.catch (do
             r <- strictDecodeCompressedFile savefile
             removeFile savefile
             case r of
               (x,y,z) -> (z :: Bool) `seq` return $ Left (x,y))
          (\ e -> case e of
                    _ -> return (Right $ "Restore failed: " ++
                                 (unwords . lines) (show e)))

