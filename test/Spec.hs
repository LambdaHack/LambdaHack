module Main (main) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T
import           Options.Applicative
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Client.UI.UIOptionsParse
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Content.RuleKind as RK
import           Game.LambdaHack.Server

import qualified Content.RuleKind
import           TieKnot

import ActorStateUnitTests
import CommonMUnitTests
import HandleHelperMUnitTests
import HandleHumanLocalMUnitTests
import InventoryMUnitTests
import ItemDescriptionUnitTests
import ItemKindUnitTests
import ItemRevUnitTests
import LevelUnitTests
import MonadClientUIUnitTests
import ReqFailureUnitTests
import SessionUIUnitTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ actorStateUnitTests
                          , commonMUnitTests
                          , handleHelperMUnitTests
                          , handleHumanLocalMUnitTests
                          , inventoryMUnitTests
                          , itemDescriptionUnitTests
                          , itemKindUnitTests
                          , itemRevUnitTests
                          , levelUnitTests
                          , reqFailureUnitTests
                          , macroTests
                          , monadClientUIUnitTests
                          , integrationTests
                          ]

integrationTests :: TestTree
integrationTests = testGroup "integrationTests" $
  -- This drives the real tieKnot, so initUI's chanFrontendIO statically
  -- references Frontend.Wasm under wasm32-wasi (to implement its
  -- "otherwise" branch) even though --frontendNull means nullStartup runs
  -- instead at runtime. That reference used to make this test fail to
  -- instantiate under `make test-wasm`'s plain wasmtime, since Frontend.Wasm
  -- needs GHC's JSFFI glue merely to be linked in, whether or not it's
  -- actually called. Works now that test-wasm runs the binary through
  -- run-wasm-test.mjs, which supplies that glue via Node.
  [ testCase "Null frontend; 5 frames" $ do
      let seed = "SMGen 131 141"
          args = words "--dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode crawl"
                 ++ [ "--setDungeonRng", seed, "--setMainRng", seed]
      serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
      tieKnot serverOptions
  ]
#ifndef USE_BROWSER
  ++
  let corule = RK.makeData Content.RuleKind.standardRules
      uiOptions = unsafePerformIO $ mkUIOptions corule defClientOptions
      testFontset :: Int -> String -> TestTree
      testFontset n fontsetName =
        testCase ("SDL frontend; init only; " ++ fontsetName ++ " fontset") $ do
          -- This test only works when run from the same directory that
          -- the .cabal file is in. And this is what Debian needs, so OK.
          -- The hacky log priority 0 tells SDL frontend to init
          -- and quit at once, for testing on CIs without graphics access.
          let seed = "SMGen " ++ show (13 + 2 * n) ++ " " ++ show (15 + 4 * n)
              args2 = words "--dbgMsgSer --logPriority 0 --newGame 3 --maxFps 100000 --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode battle"
                      ++ [ "--setDungeonRng", seed, "--setMainRng", seed
                         , "--fontset", fontsetName ]
          serverOptions2 <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args2
          tieKnot serverOptions2
  in zipWith testFontset [0..] $ map (T.unpack . fst) $ uFontsets uiOptions
#endif
