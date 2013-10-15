{-# LANGUAGE OverloadedStrings #-}
-- | Saving and restoring client game state.
module Game.LambdaHack.Client.Action.Save
  ( saveName, restoreGameCli
  ) where

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

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"

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
