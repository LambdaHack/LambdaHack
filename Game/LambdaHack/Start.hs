module Game.LambdaHack.Start
       -- ( start, speedupCops )
  where

import qualified System.Random as R
import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Control.Monad.State as MState
import qualified Data.Array.Unboxed as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Char as Char

import Game.LambdaHack.Action
import Game.LambdaHack.State
import qualified Game.LambdaHack.DungeonState as DungeonState
import qualified Game.LambdaHack.Save as Save
import Game.LambdaHack.Turn
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.ActorState
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Tile
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Actions
import Game.LambdaHack.Running
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Keybinding
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.Actor
import Game.LambdaHack.Command

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

-- TODO: move somewhere sane, probably Config.hs
-- Warning: this function changes the config file!
getGen :: Config.CP -> String -> IO (R.StdGen, Config.CP)
getGen config option =
  case Config.getOption config "engine" option of
    Just sg -> return (read sg, config)
    Nothing -> do
      -- Pick the randomly chosen dungeon generator from the IO monad
      -- and record it in the config for debugging (can be 'D'umped).
      g <- R.newStdGen
      let gs = show g
          c = Config.set config "engine" option gs
      return (g, c)

-- | Either restore a saved game, or setup a new game.
start :: Config.CP -> Session -> IO ()
start config sess@Session{scops = cops@Kind.COps{corule}} = do
  let title = rtitle $ stdRuleset corule
      pathsDataFile = rpathsDataFile $ stdRuleset corule
  restored <- Save.restoreGame pathsDataFile config title
  case restored of
    Right (msg, diary) -> do  -- Starting a new game.
      (dg, configD) <- getGen config "dungeonRandomGenerator"
      (sg, sconfig) <- getGen configD "startingRandomGenerator"
      let (DungeonState.FreshDungeon{..}, ag) =
            MState.runState (DungeonState.generate cops configD) dg
          sflavour = MState.evalState (dungeonFlavourMap (Kind.coitem cops)) ag
          state = defaultState
                    sconfig sflavour freshDungeon entryLevel entryLoc sg
          hstate = initialHeroes cops entryLoc state
      handlerToIO sess hstate diary{smsg = msg} handle
    Left (state, diary) ->  -- Running a restored a game.
      handlerToIO sess state
        diary{smsg = "Welcome back to " ++ title ++ "."}  -- TODO: save old msg?
        handle

configCommands :: Config.CP -> [(K.Key, Cmd)]
configCommands config =
  let section = Config.getItems config "commands"
      mkKey s =
        case K.keyTranslate s of
          K.Unknown _ -> assert `failure` ("unknown command key " ++ s)
          key -> key
      mkCmd s = read s :: Cmd
      mkCommand (key, def) = (mkKey key, mkCmd def)
  in L.map mkCommand section

semanticsCommands :: [(K.Key, Cmd)]
                  -> (Cmd -> Action ())
                  -> (Cmd -> String)
                  -> [(K.Key, (String, Action ()))]
semanticsCommands cmdList cmdS cmdD =
  let mkDescribed cmd =
        let semantics = if timedCmd cmd
                        then checkCursor $ cmdS cmd
                        else cmdS cmd
        in (cmdD cmd, semantics)
      mkCommand (key, def) = (key, mkDescribed def)
  in L.map mkCommand cmdList

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: Action () -> Action ()
checkCursor h = do
  cursor <- gets scursor
  slid <- gets slid
  if creturnLn cursor == slid
    then h
    else abortWith "this command does not work on remote levels"

heroSelection :: [(K.Key, (String, Action ()))]
heroSelection =
  let heroSelect k = (K.Char (Char.intToDigit k),
                      ("", selectPlayer (AHero k) >> return ()))
  in fmap heroSelect [0..9]

stdKeybinding :: Config.CP
              -> (Cmd -> Action ())
              -> (Cmd -> String)
              -> Keybinding (Action ())
stdKeybinding config cmdS cmdD =
  let section = Config.getItems config "macros"
      !kmacro = macroKey section
      cmdList = configCommands config
      semList = semanticsCommands cmdList cmdS cmdD
      moveWidth f = do
        lxsize <- gets (lxsize . slevel)
        move $ f lxsize
      runWidth f = do
        lxsize <- gets (lxsize . slevel)
        run (f lxsize, 0)
  in Keybinding
  { kcmd   = M.fromList $
             K.moveBinding moveWidth runWidth ++
             heroSelection ++
             semList ++
             [ -- debug commands, TODO: access them from a common menu or prefix
               (K.Char 'R', ("", modify toggleVision)),
               (K.Char 'O', ("", modify toggleOmniscient)),
               (K.Char 'I', ("", gets (lmeta . slevel) >>= abortWith))
             ]
  , kmacro
  , kmajor = L.map fst $ L.filter (majorCmd . snd) cmdList
  , ktimed = L.map fst $ L.filter (timedCmd . snd) cmdList
  }
