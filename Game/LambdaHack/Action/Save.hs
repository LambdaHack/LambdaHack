{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring games and player diaries.
module Game.LambdaHack.Action.Save
  ( saveGameFile, restoreGame, rmBkpSaveHistory, saveGameBkp
  ) where

import Control.Concurrent
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import qualified Data.IntMap as IM
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Config
import Game.LambdaHack.Faction
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Utils.File

-- | Save player history.
saveHistory :: FilePath -> StateDict -> IO ()
saveHistory configHistoryFile d = do
  let f (fid, (cli, loc)) =
        let faction = sfaction loc IM.! fid
            name = gname faction
        in case gAiSelected faction of
          Nothing -> Just ("Switching to " <+> name <+> ":", shistory cli)
          Just _ -> Nothing
  case catMaybes $ map f $ IM.toList d of
    [] -> return ()  -- only robots play
    [(_, h)] -> encodeEOF configHistoryFile h
    l -> encodeEOF configHistoryFile $ mergeHistory l

saveLock :: MVar ()
{-# NOINLINE saveLock #-}
saveLock = unsafePerformIO newEmptyMVar

-- | Save a simple serialized version of the current state.
-- Protected by a lock to avoid corrupting the file.
saveGameFile :: Config -> State -> StateServer -> StateDict
             -> IO ()
saveGameFile Config{configSaveFile} state ser d = do
  putMVar saveLock ()
  encodeEOF configSaveFile (state, ser, d)
  takeMVar saveLock

-- | Try to create a directory. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- in the current directory just as well.
tryCreateDir :: FilePath -> IO ()
tryCreateDir dir =
  Ex.catch
    (createDirectory dir)
    (\ e -> case e :: Ex.IOException of _ -> return ())

-- | Try to copy over data files. Hide errors due to,
-- e.g., insufficient permissions, because the game can run
-- without data files just as well.
tryCopyDataFiles :: Config -> ConfigUI -> (FilePath -> IO FilePath) -> IO ()
tryCopyDataFiles Config{ configScoresFile
                       , configRulesCfgFile }
                 ConfigUI{ configUICfgFile } pathsDataFile = do
  rulesFile  <- pathsDataFile $ takeFileName configRulesCfgFile <.> ".default"
  uiFile     <- pathsDataFile $ takeFileName configUICfgFile    <.> ".default"
  scoresFile <- pathsDataFile $ takeFileName configScoresFile
  let newRulesFile  = configRulesCfgFile <.> ".ini"
      newUIFile     = configUICfgFile    <.> ".ini"
      newScoresFile = configScoresFile
  Ex.catch
    (copyFile rulesFile newRulesFile >>
     copyFile uiFile newUIFile >>
     copyFile scoresFile newScoresFile)
    (\ e -> case e :: Ex.IOException of _ -> return ())

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGame :: Config -> ConfigUI -> (FilePath -> IO FilePath) -> Text
            -> IO (Either (State, StateServer, StateDict, History, Msg)
                          (History, Msg))
restoreGame config@Config{ configAppDataDir
                         , configSaveFile
                         , configBkpFile
                         , configHistoryFile}
            configUI -- @ConfigUI{ configHistoryFile }
            pathsDataFile title = do
  ab <- doesDirectoryExist configAppDataDir
  -- If the directory can't be created, the current directory will be used.
  unless ab $ do
    tryCreateDir configAppDataDir
    -- Possibly copy over data files. No problem if it fails.
    tryCopyDataFiles config configUI pathsDataFile
  -- If the history file does not exist, create an empty history.
  -- TODO: when history gets corrupted, start a new one, too.
  shistory <- do
    db <- doesFileExist configHistoryFile
    if db
      then strictDecodeEOF configHistoryFile
      else defHistory
  -- If the savefile exists but we get IO errors, we show them,
  -- back up the savefile and move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. If the problems are more serious,
  -- the other functions will most probably also throw exceptions,
  -- this time without trying to fix it up.
  sb <- doesFileExist configSaveFile
  bb <- doesFileExist configBkpFile
  Ex.catch
    (if sb
       then do
         renameFile configSaveFile configBkpFile
         (state, ser, d) <- strictDecodeEOF configBkpFile
         let msg = "Welcome back to" <+> title <> "."
         return $ Left (state, ser, d, shistory, msg)
       else
         if bb
           then do
             (state, ser, d) <- strictDecodeEOF configBkpFile
             let msg = "No savefile found. Restoring from a backup savefile."
             return $ Left (state, ser, d, shistory, msg)
           else do
             let msg = "Welcome to" <+> title <> "!"
             return $ Right (shistory, msg)
    )
    (\ e -> case e :: Ex.SomeException of
              _ -> let msg = "Starting a new game, because restore failed."
                             <+> "The error message was:"
                             <+> (T.unwords . T.lines) (showT e)
                   in return $ Right (shistory, msg)
    )

-- | Save the history and a backup of the save game file, in case of crashes.
-- This is only a backup, so no problem is the game is shut down
-- before saving finishes, so we don't wait on the mvar. However,
-- if a previous save is already in progress, we skip this save.
saveGameBkp :: Config -> State -> StateServer -> StateDict
            -> IO ()
saveGameBkp Config{ configSaveFile
                  , configBkpFile
                  , configHistoryFile}
            -- ConfigUI{ configHistoryFile }
            state ser d = do
  b <- tryPutMVar saveLock ()
  when b $
    void $ forkIO $ do
      saveHistory configHistoryFile d  -- save often in case of crashes
      encodeEOF configSaveFile (state, ser, d)
      renameFile configSaveFile configBkpFile
      takeMVar saveLock

-- | Remove the backup of the savegame and save the player state.
-- Should be called before any non-error exit from the game.
-- Sometimes the backup file does not exist and it's OK.
-- We don't bother reporting any other removal exceptions, either,
-- because the backup file is relatively unimportant.
-- We wait on the mvar, because saving the history at game shutdown
-- is important.
rmBkpSaveHistory :: Config -> ConfigUI -> StateDict -> IO ()
rmBkpSaveHistory Config{configBkpFile, configHistoryFile}
                 _ -- ConfigUI{configHistoryFile}
                 d = do
  putMVar saveLock ()
  saveHistory configHistoryFile d  -- save often in case of crashes
  bb <- doesFileExist configBkpFile
  when bb $ removeFile configBkpFile
  takeMVar saveLock
