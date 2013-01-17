{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring games and player diaries.
module Game.LambdaHack.Client.Action.Save
  ( saveGameCli, restoreGameCli
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
import Game.LambdaHack.Client.State
import Game.LambdaHack.Faction
import Game.LambdaHack.Msg
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
tryCopyDataFiles :: ConfigUI -> (FilePath -> IO FilePath) -> IO ()
tryCopyDataFiles ConfigUI{configUICfgFile} pathsDataFile = do
  uiFile <- pathsDataFile $ takeFileName configUICfgFile <.> ".default"
  let newUIFile     = configUICfgFile    <.> ".ini"
  Ex.catch
    (copyFile uiFile newUIFile)
    (\ e -> case e :: Ex.IOException of _ -> return ())

-- TODO: make sure it's executed eagerly.
-- | Save game to the backup savefile, in case of crashes.
-- This is only a backup, so no problem is the game is shut down
-- before saving finishes, so we don't wait on the mvar. However,
-- if a previous save is already in progress, we skip this save.
saveGameBkpCli :: ConfigUI -> State -> StateClient -> IO ()
saveGameBkpCli ConfigUI{configAppDataDirUI} s cli = do
  b <- tryPutMVar saveLock ()
  when b $
    void $ forkIO $ do
      let factionName = gname $ getSide s
          saveFile = configAppDataDirUI </> T.unpack factionName ++ ".client.sav"
          saveFileBkp = saveFile <.> ".bkp"
      encodeEOF saveFile (s, cli)
      renameFile saveFile saveFileBkp
      takeMVar saveLock

-- | Save a simple serialized version of the current state.
-- Protected by a lock to avoid corrupting the file.
saveGameCli :: Bool -> ConfigUI -> State -> StateClient -> IO ()
saveGameCli True configUI s cl = saveGameBkpCli configUI s cl
saveGameCli False ConfigUI{configAppDataDirUI} s cli = do
  putMVar saveLock ()
  let factionName = gname $ getSide s
      saveFile = configAppDataDirUI </> T.unpack factionName ++ ".client.sav"
  encodeEOF saveFile (s, cli)
  takeMVar saveLock

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGameCli :: Text -> ConfigUI -> (FilePath -> IO FilePath) -> Text
               -> IO (Either (State, StateClient, Msg) Msg)
restoreGameCli factionName configUI@ConfigUI{configAppDataDirUI}
               pathsDataFile title = do
  ab <- doesDirectoryExist configAppDataDirUI
  -- If the directory can't be created, the current directory will be used.
  unless ab $ do
    tryCreateDir configAppDataDirUI
    -- Possibly copy over data files. No problem if it fails.
    tryCopyDataFiles configUI pathsDataFile
  -- If the savefile exists but we get IO errors, we show them,
  -- back up the savefile and move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. If the problems are more serious,
  -- the other functions will most probably also throw exceptions,
  -- this time without trying to fix it up.
  let saveFile = configAppDataDirUI </> T.unpack factionName ++ ".client.sav"
      saveFileBkp = saveFile <.> ".bkp"
  sb <- doesFileExist saveFile
  bb <- doesFileExist saveFileBkp
  Ex.catch
    (if sb
     then do
       renameFile saveFile saveFileBkp
       (s, cli) <- strictDecodeEOF saveFileBkp
       let msg = "Welcome back to" <+> title <> "."
       return $ Left (s, cli, msg)
     else
       if bb
       then do
         (s, cli) <- strictDecodeEOF saveFileBkp
         let msg = "No savefile found. Restoring from a backup savefile."
         return $ Left (s, cli, msg)
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
