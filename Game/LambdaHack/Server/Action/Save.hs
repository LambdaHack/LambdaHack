{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring server game state.
module Game.LambdaHack.Server.Action.Save
  ( saveGameBkpSer, saveGameSer, restoreGameSer
  ) where

import Control.Concurrent
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Msg
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import Game.LambdaHack.Utils.File

saveLock :: MVar ()
{-# NOINLINE saveLock #-}
saveLock = unsafePerformIO newEmptyMVar

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

-- TODO: make sure it's executed eagerly.
-- | Save game to the backup savefile, in case of crashes.
-- This is only a backup, so no problem is the game is shut down
-- before saving finishes, so we don't wait on the mvar. However,
-- if a previous save is already in progress, we skip this save.
saveGameBkpSer :: Config -> State -> StateServer -> IO ()
saveGameBkpSer Config{configAppDataDir} s ser = do
  b <- tryPutMVar saveLock ()
  when b $
    void $ forkIO $ do
      let saveFile = configAppDataDir </> "server.sav"
          saveFileBkp = saveFile <.> ".bkp"
      encodeEOF saveFile (s, ser)
      renameFile saveFile saveFileBkp
      takeMVar saveLock

-- | Save a simple serialized version of the current state.
-- Protected by a lock to avoid corrupting the file.
saveGameSer :: Config -> State -> StateServer -> IO ()
saveGameSer Config{configAppDataDir} s ser = do
  putMVar saveLock ()
  let saveFile = configAppDataDir </> "server.sav"
  encodeEOF saveFile (s, ser)
  takeMVar saveLock

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGameSer :: Config -> ConfigUI -> (FilePath -> IO FilePath) -> Text
               -> IO (Either (State, StateServer, Msg) Msg)
restoreGameSer config@Config{configAppDataDir} configUI
               pathsDataFile title = do
  ab <- doesDirectoryExist configAppDataDir
  -- If the directory can't be created, the current directory will be used.
  unless ab $ do
    tryCreateDir configAppDataDir
    -- Possibly copy over data files. No problem if it fails.
    tryCopyDataFiles config configUI pathsDataFile
  -- If the savefile exists but we get IO errors, we show them,
  -- back up the savefile and move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. If the problems are more serious,
  -- the other functions will most probably also throw exceptions,
  -- this time without trying to fix it up.
  let saveFile = configAppDataDir </> "server.sav"
      saveFileBkp = saveFile <.> ".bkp"
  sb <- doesFileExist saveFile
  bb <- doesFileExist saveFileBkp
  Ex.catch
    (if sb
       then do
         renameFile saveFile saveFileBkp
         (s, ser) <- strictDecodeEOF saveFileBkp
         let msg = "Welcome back to" <+> title <> "."
         return $ Left (s, ser, msg)
       else
         if bb
           then do
             (s, ser) <- strictDecodeEOF saveFileBkp
             let msg = "No savefile found. Restoring from a backup savefile."
             return $ Left (s, ser, msg)
           else do
             let msg = "Welcome to" <+> title <> "!"
             return $ Right msg
    )
    (\ e -> case e :: Ex.SomeException of
              _ -> let msg = "Starting a new game, because restore failed."
                             <+> "The error message was:"
                             <+> (T.unwords . T.lines) (showT e)
                   in return $ Right msg
    )
