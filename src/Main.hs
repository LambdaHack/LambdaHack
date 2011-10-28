module Main where

import System.Directory
import qualified System.Random as R
import qualified Control.Monad.State as MState

import Action
import State
import DungeonState
import qualified Display
import qualified Save
import Turn
import qualified Config
import ActorAdd
import Item
import qualified Keybindings as KB

main :: IO ()
main = Display.startup start

-- | Either restore a saved game, or setup a new game.
start :: Display.InternalSession -> IO ()
start internalSession = do
  config <- Config.config
  let section = Config.getItems config "macros"
      !macros = KB.macroKey section
      sess = (internalSession, macros)
  -- check if we have a savegame
  f <- Save.file config
  b <- doesFileExist f
  restored <- if b
              then do
                     Display.displayBlankConfirm sess "Restoring save game"
                     Save.restoreGame config
              else return $ Right "Welcome to Allure of the Stars!"  -- new game
  case restored of
    Right msg  -> do
      -- TODO: move somewhere sane
      (dg, configD) <-
        case Config.getOption config "engine" "dungeonRandomGenerator" of
          Just sg ->
            return (read sg, config)
          Nothing -> do
            -- Pick the randomly chosen dungeon generator from the IO monad
            -- and record it in the config for debugging (can be 'D'umped).
            g <- R.getStdGen
            let gs = show g
                c = Config.set config "engine" "dungeonRandomGenerator" gs
            return (g, c)
      let ((ploc, lvl, dng), ag) = MState.runState (generate configD) dg
          asso = MState.evalState dungeonAssocs ag
      (sg, configS) <-
        case Config.getOption configD "engine" "startingRandomGenerator" of
          Just sg ->
            return (read sg, configD)
          Nothing -> do
            -- Pick the randomly chosen starting generator from the IO monad
            -- and record it in the config for debugging (can be 'D'umped).
            g <- R.getStdGen
            let gs = show g
                c = Config.set configD "engine" "startingRandomGenerator" gs
            return (g, c)
      let defState = defaultState dng lvl sg
          state = defState { sconfig = configS, sassocs = asso }
          hstate = initialHeroes ploc state
      handlerToIO sess hstate msg handle
    Left state ->
      handlerToIO sess state "Welcome back to Allure of the Stars." handle
