-- | Here the knot of engine code pieces and the game-specific
-- content definitions is tied, resulting in an executable game.
module TieKnot
  ( tieKnot
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified System.Random as R

import           Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Item as Item
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.SampleImplementation.SampleMonadServer (executorSer)
import           Game.LambdaHack.Server

import qualified Client.UI.Content.KeyKind as Content.KeyKind
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
--
-- The custom monad types to be used are determined by the 'executorSer'
-- and 'executorCli' calls. If other functions are used in their place
-- the types are different and so the whole pattern of computation
-- is different. Which of the frontends is run inside the UI client
-- depends on the flags supplied when compiling the engine library,
-- just as the choice of native vs JS builds.
tieKnot :: ServerOptions -> IO ()
tieKnot options@ServerOptions{sallClear, sboostRandomItem, sdungeonRng} = do
  -- This setup ensures the boosting option doesn't affect generating initial
  -- RNG for dungeon, etc., and also, that setting dungeon RNG on commandline
  -- equal to what was generated last time, ensures the same item boost.
  initialGen <- maybe R.getStdGen return sdungeonRng
  let soptionsNxt = options {sdungeonRng = Just initialGen}
      cotile = Kind.createOps Content.TileKind.cdefs
      boostedItems = Item.boostItemKindList initialGen Content.ItemKind.items
      coitem = Kind.createOps $
        if sboostRandomItem
        then Content.ItemKind.cdefs
               {content = contentFromList
                          $ boostedItems ++ Content.ItemKind.otherItemContent}
        else Content.ItemKind.cdefs
      -- Common content operations, created from content definitions.
      -- Evaluated fully to discover errors ASAP and to free memory.
      !cops = Kind.COps
        { cocave  = Kind.createOps Content.CaveKind.cdefs
        , coitem
        , comode  = Kind.createOps Content.ModeKind.cdefs
        , coplace = Kind.createOps Content.PlaceKind.cdefs
        , corule  = Kind.createOps Content.RuleKind.cdefs
        , cotile
        , coTileSpeedup = Tile.speedup sallClear cotile
        }
      -- Client content operations containing default keypresses
      -- and command descriptions.
      !copsClient = Content.KeyKind.standardKeys
  -- Wire together game content, the main loops of game clients
  -- and the game server loop.
  executorSer cops copsClient soptionsNxt
