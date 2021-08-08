module Main (main) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T
import           Options.Applicative
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.Frontend.Chosen
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Client.UI.UIOptionsParse
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Content.RuleKind as RK
import           Game.LambdaHack.Server

import qualified Content.RuleKind
import           TieKnot

import ItemKindUnitTests
import ReqFailureUnitTests
import SessionUIUnitTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [integrationTests, itemKindUnitTests, reqFailureUnitTests, macroTests]

integrationTests :: TestTree
integrationTests = testGroup "integrationTests" $
  [ testCase "Null frontend; 5 frames" $ do
      let seed = "SMGen 131 141"
          args = words "--dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode crawl"
                 ++ [ "--setDungeonRng", seed, "--setMainRng", seed]
      serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
      tieKnot serverOptions
  ]
  ++
  let corule = RK.makeData Content.RuleKind.standardRules
      uiOptions = unsafePerformIO $ mkUIOptions corule defClientOptions
      testFontset :: (Int, String) -> TestTree
      testFontset (n, fontsetName) =
        testCase ("SDL fronted; init only; " ++ fontsetName ++ " fontset") $
          when (frontendName == "sdl") $ do
            -- This test only works when run from the same directory that
            -- the .cabal files is in. And this is what Debian needs, so OK.
            -- The hacky log priority 0 tells SDL frontend to init
            -- and quit at once, for testing on CIs without graphics access.
            let seed = "SMGen " ++ show (13 + 2 * n) ++ " " ++ show (15 + 4 * n)
                args2 = words "--dbgMsgSer --logPriority 0 --newGame 3 --maxFps 100000 --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode battle"
                        ++ [ "--setDungeonRng", seed, "--setMainRng", seed
                           , "--fontset", fontsetName ]
            serverOptions2 <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args2
            tieKnot serverOptions2
      isUbuntu = ("ubuntu" `T.isPrefixOf`)
  in map testFontset $ zip [0..] $ map T.unpack $ filter (not . isUbuntu)
     $ map fst $ uFontsets uiOptions
