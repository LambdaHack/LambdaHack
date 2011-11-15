module Main where

import System.Directory
import qualified System.Random as R
import qualified Control.Monad.State as MState

import Game.LambdaHack.Action
import Game.LambdaHack.State
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Display as Display
import qualified Game.LambdaHack.Save as Save
import Game.LambdaHack.Turn
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.ActorAdd
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Keybindings as KB
import qualified Game.LambdaHack.Kind as Kind
import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.TileKind

contentOps :: Kind.COps
contentOps = Kind.COps
  { coactor = Kind.createOps Content.ActorKind.cdefs
  , cocave  = Kind.createOps Content.CaveKind.cdefs
  , coitem  = Kind.createOps Content.ItemKind.cdefs
  , cotile  = Kind.createOps Content.TileKind.cdefs
  }

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
              else return $ Right "Welcome to LambdaHack!"  -- new game
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
      let scops = contentOps
          ((ploc, lid, dng), ag) = MState.runState (generate scops configD) dg
          sflavour = MState.evalState (dungeonFlavourMap scops) ag
      (sg, sconfig) <-
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
      let defState = defaultState scops dng lid ploc sg
          state = defState{sconfig, sflavour}
          hstate = initialHeroes scops ploc state
      handlerToIO sess hstate msg handle
    Left state -> do
      let scops = contentOps
      handlerToIO sess (state {scops}) "Welcome back to LambdaHack." handle
