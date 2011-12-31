module Game.LambdaHack.Start ( start ) where

import System.Directory
import qualified System.Random as R
import qualified Control.Monad.State as MState
import qualified Data.Array.Unboxed as A

import Game.LambdaHack.Action
import Game.LambdaHack.State
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Display as Display
import qualified Game.LambdaHack.Save as Save
import Game.LambdaHack.Turn
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.ActorAdd
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Tile
import Game.LambdaHack.Command
import qualified Game.LambdaHack.Keybindings as KB
import qualified Game.LambdaHack.Kind as Kind

speedup :: Kind.Ops TileKind -> [Kind.Id TileKind -> Bool]
speedup Kind.Ops{ofoldrWithKey, obounds} =
  let createTab :: (TileKind -> Bool) -> A.UArray (Kind.Id TileKind) Bool
      createTab p =
        let f _ k acc = p k : acc
            clearAssocs = ofoldrWithKey f []
        in A.listArray obounds clearAssocs
      tabulate :: (TileKind -> Bool) -> Kind.Id TileKind -> Bool
      tabulate p = (createTab p A.!)
      isClearTab = tabulate $ kindHasFeature F.Clear
      isLitTab   = tabulate $ kindHasFeature F.Lit
  in [isClearTab, isLitTab]

speedupCops :: Kind.COps -> Kind.COps
speedupCops scops@Kind.COps{cotile=sct} =
  let ospeedup = speedup sct
      cotile = sct {Kind.ospeedup}
  in scops {Kind.cotile}

-- | Either restore a saved game, or setup a new game.
start :: Kind.COps
      -> (Cmd -> Action ())
      -> (Cmd -> Maybe String)
      -> Display.FrontendSession
      -> IO ()
start scops cmdS cmdD frontendSession = do
  let cops@Kind.COps{corule=Kind.Ops{okind, ouniqName}} = speedupCops scops
      title = rtitle $ okind $ ouniqName "standard game ruleset"
  config <- Config.config
  let section = Config.getItems config "macros"
      !macros = KB.macroKey section
      !keyb = stdKeybindings config macros cmdS cmdD
      sess = (frontendSession, cops, keyb)
  -- check if we have a savegame
  f <- Save.file config
  b <- doesFileExist f
  restored <- if b
              then do Display.displayBlankConfirmD frontendSession "Restoring save game"
                      Save.restoreGame config
              else return $ Right $ "Welcome to " ++ title ++ "!"  -- new game
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
      let ((ploc, lid, dng), ag) =
            MState.runState (generate cops configD) dg
          sflavour = MState.evalState (dungeonFlavourMap (Kind.coitem cops)) ag
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
      let defState = defaultState dng lid ploc sg
          state = defState{sconfig, sflavour}
          hstate = initialHeroes cops ploc state
      handlerToIO sess hstate msg handle
    Left state ->
      handlerToIO sess state ("Welcome back to " ++ title ++ ".") handle
