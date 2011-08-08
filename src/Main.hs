module Main where

import System.Directory
import Data.Map as M
import qualified System.Random as R

import Action
import State
import DungeonState
import qualified Display
import Random
import qualified Save
import Turn
import qualified Config
import MovableAdd
import Item
import qualified Keys as K

main :: IO ()
main = Display.startup start

-- | Either restore a saved game, or setup a new game.
start :: Display.InternalSession -> IO ()
start internalSession = do
  config <- Config.config
  let section = Config.getItems config "macros"
      !macros = K.macroKey section
      session = (internalSession, macros)
  -- check if we have a savegame
  f <- Save.file config
  b <- doesFileExist f
  restored <- if b
              then do
                     Display.displayBlankConfirm session "Restoring save game"
                     Save.restoreGame config
              else return $ Right "Welcome to Allure of the Stars!"  -- new game
  case restored of
    Right msg  -> do
      -- TODO: move somewhere sane
      case Config.getOption config "engine" "randomSeed" of
        Just seed -> R.setStdGen (R.mkStdGen seed)
        Nothing -> return ()
      (ploc, lvl, dng) <- rndToIO $ generate config
      assocs <- rndToIO $ dungeonAssocs
      let defState = defaultState dng lvl
          state = defState { sconfig = config, sassocs = assocs }
          hstate = initialHeroes ploc state
      handlerToIO session hstate msg handle
    Left state ->
      -- TODO: save and restore random seed
      handlerToIO session state "Welcome back to Allure of the Stars." handle
