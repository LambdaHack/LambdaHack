import Prelude ()

import qualified Data.Map.Strict as M
import           Options.Applicative
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.Frontend.Chosen
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Core.Prelude
import           Game.LambdaHack.Server

import qualified Client.UI.Content.Input as Content.Input

import TieKnot

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [macroTests, integrationTests]

macroTests :: TestTree
macroTests = testGroup "macroTests" $
  let coinput = IC.makeData Nothing Content.Input.standardKeysAndMouse
      stringToKeyMacro = KeyMacro . map (K.mkKM . (: []))
  in [ testCase "Macro 1 from PR description" $ do
         unwindMacros coinput (stringToKeyMacro "'j''j'")
         @?= [ ([Right []],       "'j''j'")
             , ([Left []],        "j''j'")
             , ([Left ["j"]],     "''j'")
             , ([Right ["j"]],    "'j'")
             , ([Left []],        "j'")
             , ([Left ["j"]],     "'")
             , ([Right ["j"]],    "") ]
     ]

-- The mock for macro testing.
unwindMacros :: IC.InputContent -> KeyMacro
             -> [([Either [String] [String]], String)]
unwindMacros IC.InputContent{bcmdMap, brevMap} lastPlay0 =
  let transitionMacros macros lastPlay =
        storeTrace macros lastPlay
        : case lastPlay of
          KeyMacro [] -> []
          KeyMacro (km : rest) ->
            let (macros1, lastPlay1) = case M.lookup km bcmdMap of
                  Just (_, _, HumanCmd.Record) ->
                    (fst $ recordHumanTransition macros, KeyMacro rest)
                  Just (_, _, HumanCmd.Macro ys) ->
                    macroHumanTransition ys brevMap macros (KeyMacro rest)
                  Just (_, _, HumanCmd.Repeat n) ->
                    (macros, repeatHumanTransition n macros (KeyMacro rest))
                  _ -> (macros, KeyMacro rest)
                macros2 = addToMacro brevMap km macros1
            in transitionMacros macros2 lastPlay1
      storeTrace macros lastPlay =
        let tmacros = map (either (Left . map K.showKM)
                                  (Right . map K.showKM . unKeyMacro))
                          macros
            tlastPlay = concatMap K.showKM $ unKeyMacro lastPlay
        in (tmacros, tlastPlay)
  in transitionMacros [Right (KeyMacro [])] lastPlay0

{-

2. Let `j` be an atomic action and the starting content of `slastPlay` be `[',A-b,']` and `A-b` := `[',j,']`.

   | # | smacroStack                           | slastPlay  |
   | - | ------------------------------------- | ------------ |
   | 1 | `[Right []]                  `     | `'A-b'`
   | 2 | `[Left []]                   `     | `A-b'`
   | 3 | `[Right [], Left ["A-b"]]    `     | `'j''`
   | 4 | `[Left [], Left ["A-b"]]     `     | `j''`
   | 5 | `[Left ["j"], Left ["A-b"]]  `     | `''`
   | 6 | `[Right ["j"], Left ["A-b"]] `     | `'`
   | 7 | `[Right ["A-b"]]             `     |

-}

integrationTests :: TestTree
integrationTests = testGroup "integrationTests"
  [ testCase "Null frontend; 50 frames" $ do
      let args = words "--dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 50 --automateAll --keepAutomated --gameMode crawl" ++ ["--setDungeonRng", "SMGen 123 123", "--setMainRng", "SMGen 123 123"]
      serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
      tieKnot serverOptions
  , testCase "SDL fronted; init only" $
      when (frontendName == "sdl") $ do
        -- The hacky log priority 0 tells SDL frontend to init and quit at once,
        -- for testing on CIs without graphics access.
        let args2 = words "--dbgMsgSer --logPriority 0 --newGame 3 --maxFps 100000 --benchmark --stopAfterFrames 50 --automateAll --keepAutomated --gameMode battle" ++ ["--setDungeonRng", "SMGen 125 125", "--setMainRng", "SMGen 125 125"]
        serverOptions2 <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args2
        tieKnot serverOptions2
  ]
