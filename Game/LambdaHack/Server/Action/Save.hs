-- | Saving and restoring server game state.
module Game.LambdaHack.Server.Action.Save
  ( restoreGameSer
  ) where

import Control.Concurrent
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.File

-- | Restore a saved game, if it exists. Initialize directory structure
-- and cope over data files, if needed.
restoreGameSer :: Config -> (FilePath -> IO FilePath)
               -> IO (Maybe (State, StateServer))
restoreGameSer Config{ configAppDataDir
                     , configRulesCfgFile
                     , configScoresFile }
               pathsDataFile = do
  -- Create user data directory and copy files, if not already there.
  tryCreateDir configAppDataDir
  tryCopyDataFiles pathsDataFile
    [ (configRulesCfgFile <.> ".default", configRulesCfgFile <.> ".ini")
    , (configScoresFile, configScoresFile) ]
  let saveFile = configAppDataDir </> "server.sav"
      saveFileBkp = saveFile <.> ".bkp"
  sb <- doesFileExist saveFile
  bb <- doesFileExist saveFileBkp
  when sb $ renameFile saveFile saveFileBkp
  -- If the savefile exists but we get IO or decoding errors, we show them,
  -- back up the savefile, move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. Serious IO problems (e.g. failure
  -- to create a user data directory) terminate the program with an exception.
  res <- Ex.try $
    if sb
      then do
        (s, ser) <- strictDecodeEOF saveFileBkp
        return $ Just (s, ser)
      else
        if bb
          then do
            (s, ser) <- strictDecodeEOF saveFileBkp
            let msg = "No server savefile found. "
                      ++ "Restoring from a backup savefile."
            hPutStrLn stderr msg
            return $ Just (s, ser)
          else return Nothing
  let handler :: Ex.SomeException -> IO (Maybe (State, StateServer))
      handler e = do
        let msg = "Starting a new game, because server restore failed. "
                  ++ "The error message is: "
                  ++ (unwords . lines) (show e)
        hPutStrLn stderr msg
        return Nothing
  either handler return res
