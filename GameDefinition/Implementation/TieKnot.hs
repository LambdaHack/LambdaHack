-- | Here the knot of engine code pieces, frontend and the game-specific
-- content definitions is tied, resulting in an executable game.
module Implementation.TieKnot
  ( tieKnot
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified System.Random as R

import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import qualified Game.LambdaHack.Client.UI.Content.Screen as SC
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Common.Kind
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.CaveKind as CK
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.ModeKind as MK
import qualified Game.LambdaHack.Content.PlaceKind as PK
import qualified Game.LambdaHack.Content.RuleKind as RK
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Server

import qualified Client.UI.Content.Input as Content.Input
import qualified Client.UI.Content.Screen as Content.Screen
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import           Implementation.MonadServerImplementation (executorSer)

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
--
-- The custom monad types to be used are determined by the 'executorSer'
-- call, which in turn calls 'executorCli'. If other functions are used
-- in their place- the types are different and so the whole pattern
-- of computation differs. Which of the frontends is run inside the UI client
-- depends on the flags supplied when compiling the engine library.
-- Similarly for the choice of native vs JS builds.
tieKnot :: ServerOptions -> IO ()
tieKnot options@ServerOptions{sallClear, sboostRandomItem, sdungeonRng} = do
  -- This setup ensures the boosting option doesn't affect generating initial
  -- RNG for dungeon, etc., and also, that setting dungeon RNG on commandline
  -- equal to what was generated last time, ensures the same item boost.
  initialGen <- maybe R.getStdGen return sdungeonRng
  let soptionsNxt = options {sdungeonRng = Just initialGen}
      boostedItems = IK.boostItemKindList initialGen Content.ItemKind.items
      coitem = IK.makeData $
        if sboostRandomItem
        then boostedItems ++ Content.ItemKind.otherItemContent
        else Content.ItemKind.content
      coItemSpeedup = IK.speedupItem coitem
      cotile = TK.makeData coitem Content.TileKind.content
      coTileSpeedup = Tile.speedupTile sallClear cotile
      coplace = PK.makeData cotile Content.PlaceKind.content
      cocave = CK.makeData coitem coplace cotile Content.CaveKind.content
      -- Common content operations, created from content definitions.
      -- Evaluated fully to discover errors ASAP and to free memory.
      -- Fail here, not inside server code, so that savefiles are not removed,
      -- because they are not the source of the failure.
      !cops = COps
        { cocave
        , coitem
        , comode  = MK.makeData cocave coitem Content.ModeKind.content
        , coplace
        , corule  = RK.makeData Content.RuleKind.content
        , cotile
        , coItemSpeedup
        , coTileSpeedup
        }
      -- Client content operations containing default keypresses
      -- and command descriptions.
      !ccui = CCUI
        { coinput = IC.makeData Content.Input.standardKeysAndMouse
        , coscreen = SC.makeData Content.Screen.standardLayoutAndFeatures
        }
  -- Wire together game content, the main loops of game clients
  -- and the game server loop.
  executorSer cops ccui soptionsNxt
