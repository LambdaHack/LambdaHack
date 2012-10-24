-- | Setting up game data and restoring or starting a game.
module Game.LambdaHack.Start ( start ) where

import qualified Data.Array.Unboxed as A

import Game.LambdaHack.Action
import Game.LambdaHack.State
import qualified Game.LambdaHack.Save as Save
import Game.LambdaHack.Turn
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Tile
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg

speedup :: Kind.Ops TileKind -> Kind.Speedup TileKind
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
  in Kind.TileSpeedup {isClearTab, isLitTab}

-- | Compute and insert auxiliary optimized components into game content,
-- to be used in time-critical sections of the code.
speedupCops :: Session -> Session
speedupCops sess@Session{scops = cops@Kind.COps{cotile=tile}} =
  let ospeedup = speedup tile
      cotile = tile {Kind.ospeedup}
      scops = cops {Kind.cotile}
  in sess {scops}

-- | Either restore a saved game, or setup a new game.
-- Then call the main game loop.
start :: Config.CP -> Session -> IO ()
start config slowSess = do
  let sess@Session{scops = cops@Kind.COps{ corule }} = speedupCops slowSess
      title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  restored <- Save.restoreGame pathsDataFile config title
  case restored of
    Right (diary, msg) -> do  -- Starting a new game.
      state <- gameReset config cops
      handlerToIO sess state
        diary{sreport = singletonReport msg}
        -- TODO: gameReset >> handleTurn or defaultState {squit=Reset}
        handleTurn
    Left (state, diary, msg) ->  -- Running a restored a game.
      handlerToIO sess state
        -- This overwrites the "Really save/quit?" messages.
        diary{sreport = singletonReport msg}
        handleTurn
