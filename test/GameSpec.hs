-- | The cases on the sample game's content, whose map is 80 wide: whole games
-- through 'tieKnot', and the determinism goldens, which build frames on that
-- content and play its games. They are a test executable of their own because
-- the t'Game.LambdaHack.Common.Point.Point' 'Enum' width is one value per
-- process, set here once, before anything is built, as 'tieKnot' sets it at
-- a game's startup, while the cases on the stub content, 4 wide, run under
-- @test/Spec.hs@.
module Main (main) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Options.Applicative
import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Common.Point (setSpeedupHackXSize)
import qualified Game.LambdaHack.Content.RuleKind as RK
import           Game.LambdaHack.Server

import qualified Content.RuleKind
import           TieKnot

#ifndef USE_BROWSER
import qualified Data.Text as T
import           System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Client.UI.UIOptions
import Game.LambdaHack.Client.UI.UIOptionsParse
import Game.LambdaHack.Common.ClientOptions

import DeterminismGoldenUnitTests
#endif

main :: IO ()
main = do
  setSpeedupHackXSize $ RK.rWidthMax corule
  defaultMain tests

-- | The sample game's rules.
corule :: RK.RuleContent
corule = RK.makeData Content.RuleKind.standardRules

#ifdef USE_BROWSER
-- | The determinism goldens are native only until their wasm stage.
determinismGoldenUnitTests :: TestTree
determinismGoldenUnitTests = testGroup "determinismGoldenUnitTests" []

#endif
tests :: TestTree
tests = testGroup "Tests" [ determinismGoldenUnitTests
                          , integrationTests
                          ]

integrationTests :: TestTree
integrationTests = testGroup "integrationTests" $
  -- This drives the real tieKnot, so initUI's chanFrontendIO statically
  -- references Frontend.Wasm under wasm32-wasi (to implement its "otherwise"
  -- branch) even though --frontendNull means nullStartup runs instead at
  -- runtime. That reference used to make this test fail to instantiate under
  -- `make test-wasm`'s plain wasmtime, since Frontend.Wasm needs GHC's JSFFI
  -- glue merely to be linked in, whether or not it's actually called. Works now
  -- that test-wasm runs the binary through run-wasm-test.mjs, which supplies
  -- that glue via Node.
  [ testCase "Null frontend; 5 frames" $ do
      let seed = "SMGen 131 141"
          args = words "--dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode crawl"
                 ++ [ "--setDungeonRng", seed, "--setMainRng", seed]
      serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
      void $ tieKnot serverOptions
  ]
  ++ sdlFontsetTests

-- | One SDL init-and-quit case per fontset, and none in a browser build,
-- which has no SDL frontend to initialize. Kept a binding of its own rather
-- than spliced into the list above, so that the argument of `testGroup` is
-- the same shape in both builds.
sdlFontsetTests :: [TestTree]
#ifndef USE_BROWSER
sdlFontsetTests =
  let uiOptions = unsafePerformIO $ mkUIOptions corule defClientOptions
      testFontset :: Int -> String -> TestTree
      testFontset n fontsetName =
        testCase ("SDL frontend; init only; " ++ fontsetName ++ " fontset") $ do
          -- This test only works when run from the same directory that the
          -- .cabal file is in. And this is what Debian needs, so OK. The hacky
          -- log priority 0 tells SDL frontend to init and quit at once, for
          -- testing on CIs without graphics access.
          let seed = "SMGen " ++ show (13 + 2 * n) ++ " " ++ show (15 + 4 * n)
              args2 = words "--dbgMsgSer --logPriority 0 --newGame 3 --maxFps 100000 --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode battle"
                      ++ [ "--setDungeonRng", seed, "--setMainRng", seed
                         , "--fontset", fontsetName ]
          serverOptions2 <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args2
          void $ tieKnot serverOptions2
  in zipWith testFontset [0..] $ map (T.unpack . fst) $ uFontsets uiOptions
#else
sdlFontsetTests = []
#endif
