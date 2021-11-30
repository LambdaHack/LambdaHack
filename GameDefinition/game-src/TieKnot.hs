-- | Here the knot of engine code pieces, frontend and the game-specific
-- content definitions is tied, resulting in an executable game.
module TieKnot
  ( tieKnotForAsync, tieKnot
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Exception as Ex
import qualified Data.Primitive.PrimArray as PA
import           GHC.Compact
import qualified System.Random.SplitMix32 as SM

import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import qualified Game.LambdaHack.Client.UI.Content.Screen as SC
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point (speedupHackXSize)
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.CaveKind as CK
import qualified Game.LambdaHack.Content.FactionKind as FK
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.ModeKind as MK
import qualified Game.LambdaHack.Content.PlaceKind as PK
import qualified Game.LambdaHack.Content.RuleKind as RK
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Server

import qualified Client.UI.Content.Input as Content.Input
import qualified Client.UI.Content.Screen as Content.Screen
import qualified Content.CaveKind
import qualified Content.FactionKind
import qualified Content.ItemKind
import qualified Content.ModeKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import           Implementation.MonadServerImplementation (executorSer)

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
--
-- The custom monad types to be used are determined by the @executorSer@
-- call, which in turn calls @executorCli@. If other functions are used
-- in their place- the types are different and so the whole pattern
-- of computation differs. Which of the frontends is run inside the UI client
-- depends on the flags supplied when compiling the engine library.
-- Similarly for the choice of native vs JS builds.
tieKnotForAsync :: ServerOptions -> IO ()
tieKnotForAsync options@ServerOptions{ sallClear
                                     , sboostRandomItem
                                     , sdungeonRng } = do
  -- Set the X size of the dungeon from content ASAP, before it's used.
  speedupHackXSizeThawed <- PA.unsafeThawPrimArray speedupHackXSize
  PA.writePrimArray speedupHackXSizeThawed 0 $
    RK.rWidthMax Content.RuleKind.standardRules
  void $ PA.unsafeFreezePrimArray speedupHackXSizeThawed
  -- This setup ensures the boosting option doesn't affect generating initial
  -- RNG for dungeon, etc., and also, that setting dungeon RNG on commandline
  -- equal to what was generated last time, ensures the same item boost.
  initialGen <- maybe SM.newSMGen return sdungeonRng
  let soptionsNxt = options {sdungeonRng = Just initialGen}
      corule = RK.makeData Content.RuleKind.standardRules
      boostedItems = IK.boostItemKindList initialGen Content.ItemKind.items
      itemContent =
        if sboostRandomItem
        then boostedItems ++ Content.ItemKind.otherItemContent
        else Content.ItemKind.content
      coitem = IK.makeData (RK.ritemSymbols corule)
                           itemContent
                           Content.ItemKind.groupNamesSingleton
                           Content.ItemKind.groupNames
      cotile = TK.makeData Content.TileKind.content
                           Content.TileKind.groupNamesSingleton
                           Content.TileKind.groupNames
      cofact = FK.makeData Content.FactionKind.content
                           Content.FactionKind.groupNamesSingleton
                           Content.FactionKind.groupNames
      -- Common content operations, created from content definitions.
      -- Evaluated fully to discover errors ASAP and to free memory.
      -- Fail here, not inside server code, so that savefiles are not removed,
      -- because they are not the source of the failure.
      copsRaw = COps
        { cocave = CK.makeData corule
                               Content.CaveKind.content
                               Content.CaveKind.groupNamesSingleton
                               Content.CaveKind.groupNames
        , cofact
        , coitem
        , comode = MK.makeData cofact
                               Content.ModeKind.content
                               Content.ModeKind.groupNamesSingleton
                               Content.ModeKind.groupNames
        , coplace = PK.makeData cotile
                                Content.PlaceKind.content
                                Content.PlaceKind.groupNamesSingleton
                                Content.PlaceKind.groupNames
        , corule
        , cotile
        , coItemSpeedup = speedupItem coitem
        , coTileSpeedup = Tile.speedupTile sallClear cotile
        }
  -- Evaluating for compact regions catches all kinds of errors in content ASAP,
  -- even in unused items.
  --
  -- Not using @compactWithSharing@, because it helps with residency,
  -- but nothing else and costs a bit at startup.
#ifdef USE_JSFILE
  let cops = copsRaw  -- until GHCJS implements GHC.Compact
#else
  cops <- getCompact <$> compact copsRaw
#endif
  -- Parse UI client configuration file.
  -- It is reparsed at each start of the game executable.
  -- Fail here, not inside client code, so that savefiles are not removed,
  -- because they are not the source of the failure.
  sUIOptions <- mkUIOptions corule (sclientOptions soptionsNxt)
  -- Client content operations containing default keypresses
  -- and command descriptions.
  let !ccui = CCUI
        { coinput = IC.makeData (Just sUIOptions)
                                Content.Input.standardKeysAndMouse
        , coscreen = SC.makeData corule Content.Screen.standardLayoutAndFeatures
        }
  -- Wire together game content, the main loops of game clients
  -- and the game server loop.
  executorSer cops ccui soptionsNxt sUIOptions

-- | Runs tieKnotForAsync in an async and applies the main thread workaround.
tieKnot :: ServerOptions -> IO ()
tieKnot serverOptions = do
#ifdef USE_JSFILE
  -- Hard to tweak the config file when in the browser, so hardwire.
  let serverOptionsJS = serverOptions {sdumpInitRngs = True}
  a <- async $ tieKnotForAsync serverOptionsJS
  wait a
#else
  let fillWorkaround =
        -- Set up void workaround if nothing specific required.
        void $ tryPutMVar workaroundOnMainThreadMVar $ return ()
  -- Avoid the bound thread that would slow down the communication.
  a <- async $ tieKnotForAsync serverOptions
               `Ex.finally` fillWorkaround
  -- Exit on an exception without waiting for frontend to spawn.
  link a
  -- Run a (possibly void) workaround. It's needed for OSes/frontends
  -- that need to perform some actions on the main thread
  -- (not just any bound thread), e.g., newer OS X drawing with SDL2.
  join (takeMVar workaroundOnMainThreadMVar)
  -- Wait in case frontend workaround not run on the main thread
  -- and so we'd exit too early and end the game.
  wait a
  -- Consume the void workaround if it was spurious to make @tieKnot@ reentrant.
  void $ tryTakeMVar workaroundOnMainThreadMVar
#endif
