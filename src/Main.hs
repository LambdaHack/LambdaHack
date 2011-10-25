module Main where

import System.Directory
import qualified System.Random as R

import Action
import State
import DungeonState
import qualified Display
import Random
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
      (g, configS) <-
        case Config.getOption config "engine" "startingRandomGenerator" of
          Just gs -> do
            let g = read gs
            R.setStdGen g
            return (g, config)
          Nothing -> do
            -- Record the randomly chosen starting generator for debugging.
            g <- R.getStdGen
            let gs = show g
                c = Config.set config "engine" "startingRandomGenerator" gs
            return $ (g, c)
      (ploc, lvl, dng) <- rndToIO $ generate configS
      asso <- rndToIO dungeonAssocs
      let defState = defaultState dng lvl g
          state = defState { sconfig = configS, sassocs = asso }
          hstate = initialHeroes ploc state
      handlerToIO sess hstate msg handle
    Left state -> do
      R.setStdGen (srandom state)
      handlerToIO sess state "Welcome back to Allure of the Stars." handle
