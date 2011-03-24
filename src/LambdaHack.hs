module Main where

import System.Directory
import Data.Map as M

import Action
import State
import DungeonState
import qualified Display
import Random
import qualified Save
import Turn
import qualified Config
import HeroState
import Item

main :: IO ()
main = Display.startup start

-- | Either restore a saved game, or setup a new game.
start :: Display.Session -> IO ()
start session = do
  -- check if we have a savegame
  config <- Config.config
  f <- Save.file config
  x <- doesFileExist f
  restored <- if x
              then do
                     Display.displayBlankConfirm session "Restoring save game"
                     Save.restoreGame config
              else return $ Right "Welcome to LambdaHack!"  -- new game
  case restored of
    Right msg  -> do
      (ploc, lvl, dng) <- rndToIO $ generate config
      let -- generate item associations
          assocs = M.fromList
                     [ (Potion PotionWater,   Clear),
                       (Potion PotionHealing, White) ]
          defState = defaultState dng lvl
          state = defState { sassocs = assocs, sconfig = config }
          hstate = addHeroes ploc state
      handlerToIO session hstate msg handle
    Left state ->
      handlerToIO session state "Welcome back to LambdaHack." handle
