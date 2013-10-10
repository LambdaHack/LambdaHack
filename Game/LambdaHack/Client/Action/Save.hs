{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring client game state.
module Game.LambdaHack.Client.Action.Save
  ( ChanSave, restoreGameCli, saveName, loopSave
  ) where

import Control.Concurrent
import qualified Control.Exception as Ex hiding (handle)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.State
import Game.LambdaHack.Utils.File

-- TODO: Refactor the client and server Save.hs, after
-- https://github.com/kosmikus/LambdaHack/issues/37.

type ChanSave = MVar (Maybe (State, StateClient))

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"

loopSave :: ChanSave -> IO ()
loopSave toSave =
  loop
 where
  loop = do
    -- Wait until anyting to save.
    ms <- takeMVar toSave
    case ms of
      Just (s, cli) -> do
        saveGameCli s cli
        -- Wait until the save finished. During that time, the mvar
        -- is continually updated to newest state values.
        loop
      Nothing -> return ()  -- exit

-- | Save a simple serialized version of the current state.
saveGameCli :: State -> StateClient -> IO ()
saveGameCli s cli = do
  let name = saveName (sside cli) (sisAI cli)
      saveFile = configAppDataDirUI (sconfigUI cli) </> name
  encodeEOF saveFile (s, cli)

-- | Restore a saved game, if it exists. Initialize directory structure,
-- if needed.
restoreGameCli :: String -> ConfigUI -> (FilePath -> IO FilePath) -> Text
               -> IO (Either (State, StateClient, Msg) Msg)
restoreGameCli name ConfigUI{ configAppDataDirUI
                                , configUICfgFile }
               pathsDataFile title = do
  tryCreateDir configAppDataDirUI
  tryCopyDataFiles pathsDataFile
    [(configUICfgFile <.> ".default", configUICfgFile <.> ".ini")]
  let saveFile = configAppDataDirUI </> name
  sb <- doesFileExist saveFile
  -- If the savefile exists but we get IO or decoding errors, we show them,
  -- back up the savefile, move it out of the way and start a new game.
  -- If the savefile was randomly corrupted or made read-only,
  -- that should solve the problem. Serious IO problems (e.g. failure
  -- to create a user data directory) terminate the program with an exception.
  res <- Ex.try $
    if sb
    then do
      (s, cli) <- strictDecodeEOF saveFile
      let msg = "Welcome back to" <+> title <> "."
      return $ Left (s, cli, msg)
    else do
      let msg = "Welcome to" <+> title <> "!"
      return $ Right msg
  let handler :: Ex.SomeException -> IO (Either (State, StateClient, Msg) Msg)
      handler e = let msg = "Client restore failed. The error message is:"
                            <+> (T.unwords . T.lines) (showT e)
                  in return $ Right msg
  either handler return res
